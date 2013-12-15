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

$Id: UMM.hs,v 1.73 2010/08/08 19:17:14 uwe Exp $ -}

module Main where
import Prelude hiding (putStr,putStrLn,print,readFile,getContents)
import Control.Monad
import Data.List
import Data.Maybe
import System.Exit
import System.IO.UTF8
import System.Environment.UTF8
import System.Time

import UMMHelp
import UMMData
import UMMParser
import UMMEval

processArgs :: IO (String, Command)
processArgs =
  do prog <- getProgName
     args <- getArgs
     now <- getClockTime >>= toCalendarTime
     let argc = length args
         fname = head args
         nd = genDate now
         cmd = if argc < 2
                  then BalanceCmd noName nd
                  else parseUCommand nd (intercalate " " (tail args))
     putStrLn writeHdr
     when (argc < 1 || fname == "-h" || fname == "-help" || fname == "--help")
          (putStr (usageMsg prog) >> exitWith ExitSuccess)
     when (fname == "-v" || fname == "-version" || fname == "--version")
          (exitWith ExitSuccess)
     return (fname, cmd)

getLines :: String -> IO [String]
getLines fp =
  (if fp == "-" then getContents else readFile fp) >>=
    (return . mergelines . mylines)

-- Merge explicit (ps) and implicit (qs) prices: the inputs are sorted
-- by date, newest first, and the output is the same, preferring
-- explicit over implicit prices in case of a date match.
-- For transactions, the inputs are sorted the other way: oldest first

mergePrices, mergeTrans :: [Record] -> [Record] -> [Record]

mergePrices [] qs = qs
mergePrices ps [] = ps
mergePrices pa@(p:ps) qa@(q:qs) =
  if cmpRecDate p q == LT
     then q : mergePrices pa qs
     else p : mergePrices ps qa

mergeTrans [] qs = qs
mergeTrans ps [] = ps
mergeTrans pa@(p:ps) qa@(q:qs) =
  if cmpRecDate p q == LT
     then p : mergeTrans ps qa
     else q : mergeTrans pa qs

-- Get the base currency for a given CCS, which may be itself

getBaseCurrency :: Name -> Name -> [Record] -> Name

-- old behavior: translate everything into one default currency
-- getBaseCurrency _ dc _ = dc

-- new behavior: for each CCS, translate it into that CCS' base CCS if
-- the current CCS is a derived CCS

getBaseCurrency cn _ [] = cn
getBaseCurrency cn ign ((CCSRec n _ _ nb):cs) =
  if n == cn then nb else getBaseCurrency cn ign cs
getBaseCurrency _ _ r =
  error ("internal error at getBaseCurrency! got " ++ show r)

-- Find an equivalent price in the base units for a given CCS,
-- if a suitable one can be found. If none can be found, or if the
-- ccs is already in the default units, return Nothing.

equivPrice :: CCSAmt -> Name -> Date -> [Record] -> Maybe (CCSAmt, Date)
equivPrice (CCSAmt n (Amount a)) bc date p1s =
  if n == bc then Nothing else xlat p1s
  where xlat [] = Nothing
        xlat ((PriceRec dr _ (CCSAmt nr1 (Amount ar1))
                             (CCSAmt nr2 (Amount ar2))):ps) =
          if date >= dr && n == nr1 && nr2 == bc
             then Just ((CCSAmt bc (Amount (a*ar2/ar1))), dr)
             else xlat ps
        xlat (_:ps) = xlat ps		-- for non-PriceRec records

-- Translate price if possible

reprice :: CCSAmt -> Name -> [Record] -> Date -> [Record] -> CCSAmt
reprice c@(CCSAmt cn _) bc ccs date prices =
  maybe c fst (equivPrice c (getBaseCurrency cn bc ccs) date prices)

-- Pretty-print accounts

ppAccts :: [(Name, [String])] -> Int -> [String]
ppAccts es sp =
  concatMap (ppe (sp + maximum (map (length . getN . fst) es))) es
  where getN (Name n) = n
        ppe l (m,as) =
          let isp = concat (repeat " ")
          in zipWith gl (take l (show m ++ isp) : repeat (take l isp)) as
        gl a b = concat [a, b]

showPos :: Name -> [Record] -> Date -> [Record] ->
           AccountData -> [(Name, [String])]
showPos dc ccs da ps as = map f1 as
  where f1 (n1,_,es) = (n1, if null es then ["[empty]"] else map f2 es)
        f2 c2@(CCSAmt c2n _) =
          let sv = show c2
              ep = equivPrice c2 (getBaseCurrency c2n dc ccs) da ps
              jep = fromJust ep
              CCSAmt n (Amount a) = fst jep
              jer = CCSAmt n (Amount (roundP 2 a))
              sd d = if d == startTime then "" else " (" ++ show d ++ ")"
              sp = "\t~" ++ show jer ++ sd (snd jep)
              pad = ' ' : concat (replicate (18 - length sv) " ")
          in if isJust ep then sv ++ pad ++ sp else sv

selAccts :: Bool -> [Name] -> AccountData -> AccountData
selAccts keep names accs = f2 (f1 accs)
  where f0 = filter (\a -> elem (tr1 a) names)
        f1 = if length names == 1 && head names == noName then id else f0
        f2 = filter (\a -> (if keep then not . tr2 else const False) a ||
                           not (null (tr3 a)))
        tr1 (v,_,_) = v
        tr2 (_,v,_) = v
        tr3 (_,_,v) = v

-- Turn an account-group into a list of accounts. An account-group can
-- contain (names of) other account-groups, including recursively, and
-- loops aren't prohibited either... need to handle all those cases.

expandGroup :: [Record]  -> [Record] -> Name -> [Name]
expandGroup aas ggs ag =
  if ag == noName || ag == todoName
     then [ag]
     else ff [ag] (map getRecName aas) (map ggt ggs) [] []
  where ff [] _ _ am _ = am
        ff (q:qs) as gs am ab
          | elem q am || elem q ab  = ff qs as gs am ab
          | elem q as               = ff qs as gs (q:am) ab
          | isJust (lookup q gs)    = ff (qs ++ fromJust (lookup q gs))
                                         as gs am (q:ab)
          | otherwise               = error ("unknown account or group! "
                                              ++ show q)
        ggt (GroupRec n as) = (n,as)
        ggt r = error ("internal error at expandGroup! got " ++ show r)

doList :: CmdOpt -> Name -> [Record] -> [Record] ->
          [Record] -> [Record] -> [Record] -> IO ()
doList w dc ccs accts grps incs exps =
  do when (chk w COLCCS)
       (putStrLn "# Currencies, Commodities, Securities\n" >>
        putStrLn ("# default ccs " ++ show dc ++ "\n") >> sh ccs)
     when (chk w COLAccs) (putStrLn "\n# Accounts\n"   >> sh accts)
     when (chk w COLGrps) (putStrLn "\n# Groups\n"     >> sh grps)
     when (chk w COLIncs) (putStrLn "\n# Incomes\n"    >> sh incs)
     when (chk w COLExps) (putStrLn "\n# Expenses\n"   >> sh exps)
  where chk w1 w2 = w1 == w2 || w1 == COLAll
        sh = mapM_ print

-- Generate the sum of a list of amounts, translating them
-- into the default ccs to the extent possible

genSum :: Name -> [Record] -> [Record] -> Date -> [CCSAmt] -> [CCSAmt]
genSum dc ccs prices date vs =
  let fp = map (\e -> reprice e dc ccs date prices) vs
      gp = groupBy eqCCSAmtName (sortBy cmpCCSAmtName fp)
  in filter (\e -> ccsA e /= 0) (map sumCCS gp)
  where sumCCS cs =
           CCSAmt (ccsN (head cs)) (Amount (roundP 2 (sum (map ccsA cs))))
        ccsN (CCSAmt n _) = n
        ccsA (CCSAmt _ (Amount a)) = a

doBalance :: Bool -> Date -> [Name] -> Name -> [Record] ->
             [Record] -> [Record] -> [Record] -> IO ()
doBalance ke date names dc ccs accts trans prices =
  do final <- getBalances startTime date Nothing False accts trans
     let fsel = selAccts ke names final
         sp = genSum dc ccs prices date (concatMap tr3 fsel)
     putStrLn ("Account balances as of " ++ gregorianDateToWDay date
              ++ " " ++ show date)
     mapM_ putStrLn (ppAccts (showPos dc ccs date prices fsel) 8)
     putStrLn ("Grand total: ~" ++ show sp)
  where tr3 (_,_,v) = v

doRegister :: Date -> Date -> [Name] -> Name -> [Record] ->
              [Record] -> [Record] -> [Record] -> Bool -> IO ()
doRegister d1 d2 names dc ccs accts trans prices dorec =
  do final <- getBalances d1 d2 (Just names) dorec accts trans
     putStrLn ((if dorec then "Reconciled" else "Account")
               ++ " balance as of " ++ show d2)
     mapM_ putStrLn (ppAccts (showPos dc ccs d2 prices
                                      (selAccts True names final)) 8)

doChange :: Bool -> Date -> Date -> Name -> Name ->
            [Record] -> [Record] -> [Record] -> IO ()
doChange verbose d1 d2 name dc ccs accts trans =
  do let aux = if notElem name (map getRecName accts)
                  then [AccountRec name d1 False "" Nothing]
                  else []
         trs = dropWhile (\t -> getRecDate t <= d1) trans
         mn = if verbose then Just [name] else Nothing
     final <- getBalances d1 d2 mn False (aux ++ accts) trs
     putStr "Change"
     when (d1 /= startTime) (putStr (" from " ++ show d1))
     putStrLn (" to " ++ show d2)
     mapM_ putStrLn (ppAccts (showPos dc ccs d2 []
                                      (selAccts True [name] final)) 8)

-- Filter predicate for computing basis: want to only consider transactions
-- which involve ccs "n0"

hasCur :: Name -> Record -> Bool
hasCur c (XferRec _ _ _ tos _ _) = any hC tos
  where hC (_, CCSAmt n _) = c == n
hasCur c (ExchRec _ _ _ _ (CCSAmt n1 _) (CCSAmt n2 _) _) = c == n1 || c == n2
hasCur c (SplitRec _ n1 _ _) = c == n1
hasCur _ (NoteRec _ _ _ _) = False
hasCur _ r = error ("internal error at hasCur! got " ++ show r)

main :: IO ()
main =
  do (file, action) <- processArgs
     recs <- getLines file >>= mapM (return . parseURecord) >>= validateRecs
     let (dc, cb, c1, incs, exps, a1, grps, tr1, per, pr1) = classifyRecs recs
     cd <- validateCCS dc cb c1
     let ccs = cb ++ cd
     accts <- validateAccts dc ccs a1
     validateTransPrices ccs incs exps accts (tr1 ++ per ++ pr1)
     let tr2 = expandRecurringTrans per
         trans = mergeTrans tr1 tr2
         pr2 = generateImplicitPrices dc trans cd
         prices = mergePrices (reverse pr1) pr2
         egrp = expandGroup accts grps
         pse r = putStrLn (showExp r) >> putStrLn ""
     case action of
       ChangeCmd verbose name date1 date2 ->
         doChange verbose date1 date2 name dc ccs accts trans
       BalanceCmd name date ->
         doBalance True date (egrp name) dc ccs accts trans prices
       BasisCmd name date ->
         doBalance False date [noName] dc ccs accts
                   (filter (hasCur name) trans) []
       ExportCmd -> mapM_ pse trans >> mapM_ pse (reverse prices)
       ListDataCmd w ->
         doList w dc ccs accts grps incs exps
       PlotCmd name date1 date2 (Name output) ->
         let crec = find (\r -> getRecName r == name) cd
             gs = genSum1 dc ccs prices
             auxa = AccountRec name date1 False "" Nothing
             auxt1 = dropWhile (\t -> getRecDate t <= date1) trans
             auxt2 = mergeTrans trans (reverse prices)
         in if elem name (map getRecName cb)
               then putStrLn (show name ++ " is a base CCS!")
               else if isNothing crec
                       then if elem name (map getRecName (incs ++ exps))
                               then plotBalances date1 date2 name [name]
                                                 (auxa : accts) auxt1 output gs
                               else plotBalances date1 date2 name (egrp name)
                                                 accts auxt2 output gs
                       else plotPrices name (getNB (fromJust crec))
                                       date1 date2 prices output
       PriceCmd name date1 date2 ->
         let crec = find (\r -> getRecName r == name) cd
         in if elem name (map getRecName cb)
               then putStrLn (show name ++ " is a base CCS!")
               else if isNothing crec
                       then putStrLn ("Error! unknown CCS " ++ show name)
                       else getPrices name (getNB (fromJust crec))
                                      date1 date2 prices
       RegisterCmd name date1 date2 ->
         doRegister date1 date2 (egrp name) dc ccs accts trans prices False
       ReconcileCmd name date ->
         doRegister startTime date (egrp name) dc ccs accts trans prices True
       ToDoCmd date ->
         getBalances startTime date Nothing False accts trans >> return ()
  where getNB (CCSRec _ _ _ nb) = nb
        getNB r = error ("internal error at main! got " ++ show r)
        genSum1 dc ccs prices (r,bs) =
          let date = getRecDate r
          in (date, map ccsA (genSum dc ccs prices date bs))
        ccsA (CCSAmt _ a) = a
