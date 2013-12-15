{- Copyright 2010 Uwe Hollerbach <uh@alumni.caltech.edu>

This file is part of umm, Uwe's Money Manager.

umm is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your
option) any later version.

umm is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with umm; if not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

$Id: UMMPlot.hs,v 1.10 2010/08/08 18:23:11 uwe Exp $ -}

module UMMPlot (genPlot) where
import Prelude hiding (putStr, putStrLn, print)
import Data.List
import System.IO(withFile, IOMode(..))
import System.IO.UTF8
-- Just one of these is needed to build this; pick the appropriate one
-- import System.Cmd	-- for ghc 6.8
import System.Process	-- for ghc 6.10 (and newer? untested)
import Control.Monad

import UMMData

-- TODO: a lot more work!
-- Do we need the niceBounds stuff at all? Or can gnuplot handle it?

-- Given a pair of numbers, return a set of nice bounds plus a number of
-- sub-intervals into which those nice bounds should be divided:
--
--	niceBounds 23 65 -> (20.0,65.0,9)
--
-- meaning that the nice lower and upper bounds are 20 and 65, and that
-- interval should be sub-divided into 9 sub-intervals which will thus
-- each have length 5, and therefore tic-marks will be placed at nice
-- round numbers: [20, 25, ..., 60, 65]
--
-- The original algorithm is not mine, but I don't now remember
-- where it came from... I got it a long time ago.

-- TODO: make this polymorphic, accepting Fractional or even Num?

niceBounds :: Double -> Double -> (Double, Double, Int)
niceBounds l h | l == h      = (l - 1, h + 1, 8)
               | l > h       = nbw h l
               | otherwise   = nbw l h
  where nbw lo hi =
          let sc = fsc (hi - lo) 1.0
              los = lo*sc
              his = hi*sc
              tries = map (fn los his sc) [1.0, 0.5, 0.25, 0.2, 0.1, 0.05]
          in head (dropWhile (\(_,_,n) -> n <= 6) tries)
        fsc d s | d < 1       = fsc (10*d) (10*s)
                | d > 10      = fsc (d/10) (s/10)
                | otherwise   = s
        fn lo hi sc e =
          let fl = fromInteger (floor (lo/e))
              ch = fromInteger (ceiling (hi/e))
              n = ceiling (hi/e) - floor (lo/e)
              es = e/sc
          in (fl*es, ch*es, n)

-- Various steps to take in subdividing a date range, used below

data DateStep = Day | Month | YearM | YearQ | Year | DY Int deriving (Show)

-- Given a pair of dates, return another pair of dates which are nice bounds
-- plus a subdivision indicator: analogous to niceBounds, above, but
-- specialized for dates, for which what's "nice" is slightly different
-- than for generic numbers.

niceDateBounds :: Date -> Date -> (Date, Date, DateStep)
niceDateBounds l h = if l <= h then njbw l h else njbw h l
  where njbw (Date yl ml _) (Date yh mh _) =
          let dy = yh - yl
              lo1 = nd yl ml 1
              hi1 = nd yh (mh + 1) 0
              lo2 = nd yl (ml - rem (ml - 1) 3) 1
              hi2 = nd yh (mh + 3 - rem (mh - 1) 3) 0
              lo3 = nd yl 1 1
              hi3 = nd yh 12 31
              (l4, h4, n4) =
                niceBounds (fromIntegral yl) (fromIntegral (yh + 1))
              lo4 = nd (fromInteger (round l4)) 1 1
              hi4 = nd (fromInteger (round h4)) 1 0
          in if dy <= 0
                then (lo1, hi1, if ml == mh then Day else Month)
                else if dy <= 2
                        then (lo1, hi1, YearM)
                        else if dy <= 5
                                then (lo2, hi2, YearQ)
                                else if dy <= 15
                                        then (lo3, hi3, Year)
                                        else (lo4, hi4, DY n4)
-- The round-tripping of Date to Julian day back to Date may seem stupid,
-- but it normalizes the date: for example, 2009-13-0 becomes 2009-12-31
-- without having to worry about how many days there are in a given month
        nd y m d = gregorianDate (julianDate (Date y m d))

-- Adjust as needed: for example, for pgm output, use
--  "set terminal pbm gray medium size 800,600\n" ++

plot_cmds :: String -> String -> Date -> Date -> String
plot_cmds output name lo hi =
  "set terminal postscript 'Times-Roman' 16\n" ++
  "set output '" ++ output ++ ".ps'\n" ++
  "set title 'Value of account \"" ++ name ++ "\" over time'\n" ++
  "unset key\n" ++
  "set xdata time\n" ++
  "set timefmt \"%Y-%m-%d\"\n" ++
  "set format x \"%Y-%m-%d\"\n" ++
  "set xrange [\"" ++ show lo ++ "\":\"" ++ show hi ++ "\"]\n" ++
  "plot '" ++ output ++ ".dat' using 1:2 with lines\n"

-- If we don't specify a range in plotting data, we get the default,
-- which is "beginning of time to now"; that produces a not-very-useful
-- graph. Since I don't want to necessarily always auto-snap to the
-- dates given by the data (sometimes I want to show a 5-year graph of
-- partial data, in order to combine it later with other, more-complete,
-- data), there's a bit of hackery required: that's the "d1 = ..." stuff.

genPlot :: String -> Name -> Date -> Date -> [(Date, [Amount])] -> IO ()
genPlot output name date1 date2 pts =
  let pts1 = filter (not . null . snd) pts
      d1 = if date1 == startTime then foldl1 min (map fst pts1) else date1
      shn = show name
  in if null pts1
        then putStrLn ("Nothing to show while trying to plot " ++ shn)
        else do let (nlo, nhi, _) = niceDateBounds d1 date2
                withFile (output ++ ".plot") WriteMode
                  (\h -> hPutStr h (plot_cmds output shn nlo nhi))
                withFile (output ++ ".dat") WriteMode
                  (\h -> mapM_ (dP h) pts1)
                doit ("gnuplot " ++ output ++ ".plot")
  where dP fp (d,vs) =
          hPutStr fp (show d) >> mapM_ (dY fp) vs >> hPutStrLn fp ""
        dY fp r = hPutStr fp (' ' : show r)

doit :: String -> IO ()
doit cmd = putStrLn ("running '" ++ cmd ++ "'") >> system cmd >> return ()
