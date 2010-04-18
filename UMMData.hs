{- Copyright 2009-2010 Uwe Hollerbach <uh@alumni.caltech.edu>

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

$Id: UMMData.hs,v 1.49 2010/01/02 22:33:34 uwe Exp $ -}

module UMMData (Name(..), Date(..), Amount(..), startTime,
                Command(..), CmdOpt(..), Record(..), genDate,
                getRecDate, cmpRecDate, getRecName, cmpRecName,
                Ledger(..), runLedger, getResult, getInfo, getErrs,
                recordInfo, recordErr, recordNil, showExp, BSE(..),
                CCSAmt(..), cmpCCSAmtName, eqCCSAmtName, AccountData,
                noName, todoName, joinDrop, roundP, isLeap, validDate,
                trimspace, mylines, mergelines, uniqAdjBy, uniqAdj) where
import Prelude
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import System.Time

-- UMM values: names, dates, amounts, records

newtype Name = Name String deriving (Eq, Ord)
instance Show Name where
  show (Name name) = name

noName, todoName, nilName :: Name

noName = Name ""
todoName = Name " todo "
nilName = Name " nil "

data Date = Date Int Int Int deriving (Eq)

instance Show Date where
  show (Date y m d) = show y ++ "-" ++ show2 m ++ "-" ++ show2 d
    where show2 n = reverse (take 2 (reverse (show n) ++ "00"))

instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2) =
    let dy = compare y1 y2
        dm = compare m1 m2
    in if dy /= EQ then dy else if dm /= EQ then dm else compare d1 d2

genDate :: CalendarTime -> Date
genDate (CalendarTime {ctYear = y, ctMonth = m, ctDay = d}) =
  let m2 = case m of
             January   -> 1
             February  -> 2
             March     -> 3
             April     -> 4
             May       -> 5
             June      -> 6
             July      -> 7
             August    -> 8
             September -> 9
             October   -> 10
             November  -> 11
             December  -> 12
  in Date y m2 d

startTime :: Date
startTime = Date (-4003) 10 23		-- 9 AM (GMT)

newtype Amount = Amount Rational deriving (Eq, Ord)
instance Show Amount where
  show (Amount amt) = if amt < 0 then '-' : shRat (-amt) else shRat amt

shRat :: Rational -> String
shRat val =
  let n = numerator val
      d = denominator val
      (p2,r2) = divout d 2
      (p5,r5) = divout r2 5
      e10 = max p2 p5
      vs1 = show (numerator (val * 10^e10))
      l1 = max (length vs1) (1 + e10)
      vs2 = take l1 (reverse vs1 ++ cycle "0")
      vsf = reverse (take e10 vs2)
      vsi = reverse (drop e10 vs2)
  in if r5 == 1
        then if e10 == 0 then show n else vsi ++ "." ++ vsf
        else show n ++ "/" ++ show d

divout :: Integer -> Integer -> (Int, Integer)
divout n q = doit 0 n
  where doit e v = if rem v q == 0 then doit (e + 1) (quot v q) else (e,v)

data CCSAmt = CCSAmt Name Amount
instance Show CCSAmt where
  show (CCSAmt name amount) = joinDrop [show amount, show name]

-- Compare two CCSAmts by name

cmpCCSAmtName :: CCSAmt -> CCSAmt -> Ordering
cmpCCSAmtName (CCSAmt n1 _) (CCSAmt n2 _) = compare n1 n2

eqCCSAmtName :: CCSAmt -> CCSAmt -> Bool
eqCCSAmtName (CCSAmt n1 _) (CCSAmt n2 _) = n1 == n2

-- All accounts: an array of tuples of names and amounts
-- TODO: make this a newtype?

type AccountData = [(Name, [CCSAmt])]

data CmdOpt = COLAll
            | COLCCS
            | COLAccs
            | COLGrps
            | COLIncs
            | COLExps
  deriving (Eq)

instance Show CmdOpt where
  show COLAll = "all"
  show COLCCS = "ccs"
  show COLAccs = "accounts"
  show COLGrps = "groups"
  show COLIncs = "incomes"
  show COLExps = "expenses"

data Command = ListDataCmd CmdOpt
             | ToDoCmd Date
             | BalanceCmd Name Date
             | BasisCmd Name Date
             | RegisterCmd Name Date Date
             | ReconcileCmd Name Date
             | PriceCmd Name Date
             | ChangeCmd Bool Name Date Date
             | ExportCmd

instance Show Command where
  show (ListDataCmd opt) = joinDrop ["list", show opt]
  show (ToDoCmd date) = joinDrop ["todo", show date]
  show (PriceCmd name date) = joinDrop ["price", show name, show date]
  show (BalanceCmd name date) = joinDrop ["balance", show name, show date]
  show (BasisCmd name date) = joinDrop ["basis", show name, show date]
  show (RegisterCmd name date1 date2) =
    joinDrop ["register", show name, show date1, show date2]
  show (ReconcileCmd name date) = joinDrop ["reconcile", show name, show date]
  show (ChangeCmd verbose name date1 date2) =
    joinDrop ["change", shIf verbose "verbose",
              show name, show date1, show date2]
  show ExportCmd = "export"

-- No, not bovine spongiform encephalitis! Disambiguate buy, sell, and
-- exch records: internally, they are all treated as exch, because
-- that makes the code simple, but the user can enter "buy" or "sell",
-- and it seems only polite to echo that back on output.

data BSE = B | S | E deriving (Eq)

instance Show BSE where
  show B = "buy"
  show S = "sell"
  show E = "exch"

data Record = CCSRec Name String (Maybe Amount) Name
            | IncomeRec Name String
            | ExpenseRec Name String
            | AccountRec Name Date String (Maybe CCSAmt)
            | GroupRec Name [Name]
            | PriceRec Date Bool CCSAmt CCSAmt
            | XferRec Date Bool Name [(Name, CCSAmt)] String String
            | ExchRec BSE Date Bool Name CCSAmt CCSAmt String
            | SplitRec Date Name Amount Amount
            | CommentRec String
            | ToDoRec Date Bool String
            | ErrorRec String

instance Show Record where
  show = showR

shIf :: Bool -> String -> String
shIf fl val = if fl then val else ""

shM :: Show a => Maybe a -> String
shM val = if isJust val then show (fromJust val) else ""

shDef :: String -> String -> String
shDef s d = if s == "" then d else s

-- We use "show str" here to add quotes and escape any escapables

optStr :: String -> String
optStr str = shIf (str /= "") (show str)

shRec :: Bool -> String
shRec rec = shIf rec "*"

joinDrop :: [String] -> String
joinDrop = intercalate " " . filter (/= "")

showR :: Record -> String
showR (CCSRec n d ma nb) =
  joinDrop ["ccs", show n, optStr d, shM ma, show nb]
showR (IncomeRec n d) = joinDrop ["income", show n, optStr d]
showR (ExpenseRec n d) = joinDrop ["expense", show n, optStr d]
showR (AccountRec n da de mi) =
  joinDrop ["account", show n, show da, optStr de, shM mi]
showR (GroupRec n as) = joinDrop (["group", show n] ++ map show as)
showR (PriceRec d imp c1 c2) =
  joinDrop [shIf imp "[", "price", show d, show c1, show c2, shIf imp "]"]

showR (XferRec d r from tos m i) =
  joinDrop ["xfer", shRec r, show d, show from, showTos tos, optStr m, i]

showR (ExchRec t d r a c1 c2 memo) =
  let hs = if t == S then [show c2, show c1] else [show c1, show c2]
  in joinDrop ([show t, shRec r, show d, show a] ++ hs ++ [optStr memo])

showR (SplitRec d n a1 a2) =
  joinDrop ["split", show d, show n, show a1, show a2]

showR (CommentRec c) = shIf (c /= "") (joinDrop ["#", c])

showR (ToDoRec d r memo) =
  joinDrop ["todo", shRec r, show d, shDef memo "something! but what?"]

showR (ErrorRec str) = joinDrop ["#err", str]

showTos :: [(Name, CCSAmt)] -> String
showTos [] = "{}"
showTos (t:[]) = showTo1 False t
showTos ts = "{" ++ intercalate ", " (map (showTo1 False) ts) ++ "}"

showTo1 :: Bool -> (Name, CCSAmt) -> String
showTo1 sp (n,a) = joinDrop [show n, shIf sp " ", show a]

showExp :: Record -> String
showExp (XferRec d r from tos m i) =
  let l1 = joinDrop [show d, shRec r, i, shDef m "transfer"]
      l2 = map (showTo1 True) tos
      l3 = joinDrop [show from]
  in intercalate "\n    " ([l1] ++ l2 ++ [l3])

showExp (ExchRec _ d r a c1 (CCSAmt n2 (Amount a2)) m) =
  let l1 = joinDrop [show d, shRec r, shDef m "exchange"]
      l2 = joinDrop [show a, ' ' : show c1]
      l3 = joinDrop [show a, ' ' : show (CCSAmt n2 (Amount (-a2)))]
      l4 = "the:world"
  in intercalate "\n    " [l1, l2, l3, l4]

showExp (PriceRec d _ (CCSAmt n1 (Amount a1)) (CCSAmt n2 (Amount a2))) =
  joinDrop ["P", show d, show n1,
            ' ' : show (CCSAmt n2 (Amount (roundP 4 (a2/a1))))]

showExp (SplitRec _ _ _ _) = "# Split to be implemented"
showExp r@(ToDoRec _ _ _)  = "# " ++ showR r
showExp r = error ("internal error at showExp! got " ++ show r)

-- Get the date (or at any rate /some/ date) from a Record

getRecDate :: Record -> Date
getRecDate (AccountRec _ d _ _) =    d
getRecDate (PriceRec d _ _ _) =      d
getRecDate (XferRec d _ _ _ _ _) =   d
getRecDate (ExchRec _ d _ _ _ _ _) = d
getRecDate (SplitRec d _ _ _) =      d
getRecDate (ToDoRec d _ _) =         d
getRecDate _ = startTime	-- so it works for every Record

-- Get the name (or at any rate /some/ name) from a Record

getRecName :: Record -> Name
getRecName (CCSRec n _ _ _) =     n
getRecName (IncomeRec n _) =      n
getRecName (ExpenseRec n _) =     n
getRecName (AccountRec n _ _ _) = n
getRecName (GroupRec n _) =       n
getRecName _ = nilName		-- so it works for every Record

-- Compare two Records
cmpRecDate, cmpRecName :: Record -> Record -> Ordering

-- by date
cmpRecDate = comparing getRecDate

-- by name
cmpRecName = comparing getRecName

-- Ledger monad: essentially a parametrized bi-level version of the Logger
-- monad from RWH. The first component of the tuple is "the final result",
-- whatever that may be, the second component is a list of ordinary bits
-- of information, and the third component is a list of extraordinary bits
-- of information, aka errors.

newtype Ledger e i r = Ledger (r, [i], [e]) deriving (Show)

runLedger :: Ledger e i r -> (r,[i],[e])
runLedger (Ledger a) = a

getResult :: Ledger e i r -> r
getResult (Ledger (r,_,_)) = r

getInfo :: Ledger e i r -> [i]
getInfo   (Ledger (_,i,_)) = i

getErrs :: Ledger e i r -> [e]
getErrs   (Ledger (_,_,e)) = e

-- Record a message

recordInfo :: i -> Ledger e i ()
recordInfo i = Ledger ((), [i], [])

-- Record an error

recordErr :: e -> Ledger e i ()
recordErr e = Ledger ((), [], [e])

-- Record nothing: a placeholder for conditionals:
-- if someCond then recordErr "someErr" else recordNil

recordNil :: Ledger e i ()
recordNil = Ledger ((), [], [])

-- Note that because we use append (++) for lists here, this is fairly
-- low-performance: it's entirely ok for this application, where we
-- toss around a few to a few thousand list entries, but for longer
-- lists it might not be suitable. <shrug>

instance Monad (Ledger e i) where
  return a = Ledger (a, [], [])
  (>>=) m k =
    let (a,li1,le1) = runLedger m
        n = k a
        (b,li2,le2) = runLedger n
    in Ledger (b, li1 ++ li2, le1 ++ le2)
  (>>) a f = a >>= const f

-- Some miscellany

-- Round a rational number val to np decimal places
-- This might not be ultimately exact, but it's for converted prices
-- which might be a couple of days out of date anyway, so no big deal

roundP :: Int -> Rational -> Rational
roundP np val =
  if val < 0 then negate (rp (negate val)) else rp val
  where rp v1 =
          let e1 = 10 ^ max 0 np
              e2 = 10*e1
              vs = v1 * fromInteger e2
              n = numerator vs
              d = denominator vs
              q1 = quot n d
              q2 = quot (q1 + 4) 10
          in q2 % e1

-- divisible by 4 except if divisible by 100 except if divisible by 400

isLeap :: Int -> Bool
isLeap y = rem y 4 == 0 && (rem y 100 /= 0 || rem y 400 == 0)

-- Check the validity of a date: see that the day and month are in bounds

validDate :: Date -> Bool
validDate (Date y m d) =
  let lim = [ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ] !! m
  in (m >= 1 && m <= 12 && d >= 1 &&
       (d <= lim || m == 2 && isLeap y && d <= 29))

-- Remove leading and trailing whitespace from a string.

trimspace :: String -> String
trimspace str = f (f str)
  where f = dropWhile isSpace . reverse

-- A version of 'lines' which deals with all combinations of '\r' and '\n'.
-- It drops blank lines and trims leading and trailing whitespace from
-- lines, which is all perfectly fine here, but which might be a problem
-- elsewhere.

mylines :: String -> [String]
mylines str = filter (/= "") (ml str)
  where ml [] = []
        ml s  = let (p, s1) = break (\c -> c == '\n' || c == '\r') s
                in trimspace p : (if null s1 then [] else ml (tail s1))

-- Re-join continuation lines approximately following haskell convention: a
-- trailing '\' as the last character on a line, followed by a line with a
-- leading '\' as the first character, means that the two '\' are dropped
-- and the two lines joined. In conjunction with the trimspace aspect of
-- mylines, above, this pretty much replicates the haskell way.

isL, isF :: String -> Bool
isL s = not (null s) && last s == '\\'
isF s = not (null s) && head s == '\\'

mergelines :: [String] -> [String]
mergelines [] = []
mergelines (h:[])
  | isL h || isF h     = error ("mergelines: partial line " ++ h)
  | otherwise          = [h]
mergelines (h:t:ts)
  | isL h && isF t     = mergelines ((init h ++ tail t):ts)
  | isL h || isF h     = error ("mergelines: partial line " ++ h)
  | otherwise          = h : mergelines (t:ts)

-- Examine a list of items: with a comparison function which returns
-- True if two items are equal, False otherwise, determine if there are
-- duplicate adjacent entries in the list. Return True if all items are
-- unique, False if there are adjacent duplicates. The restriction to
-- adjacent elements makes it faster than 'nub' or 'elem' or the like.

uniqAdjBy :: (a -> a -> Bool) -> [a] -> Bool
uniqAdjBy _ [] = True
uniqAdjBy _ [_] = True
uniqAdjBy f (v1:v2:[]) = not (f v1 v2)
uniqAdjBy f (v1:v2:vs) = if f v1 v2 then False else uniqAdjBy f (v2:vs)

uniqAdj :: (Eq a) => [a] -> Bool
uniqAdj = uniqAdjBy (==)
