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

$Id: UMMEval.hs,v 1.44 2010/05/10 04:06:46 uwe Exp $ -}

module UMMEval (validateRecs, validateCCS, validateAccts, classifyRecs,
                validateTransPrices, generateImplicitPrices, getBalances,
                getPrices, expandRecurringTrans) where
import Prelude hiding (putStr,putStrLn,print)
import Data.List
import Data.Maybe
import System.IO.UTF8
import Control.Monad

import UMMData

-- Internal error: complain loudly!

intErr :: (Show i) => String -> i -> o
intErr loc w =
  error ("internal error at " ++ loc ++ "\ngot unexpected " ++ show w)

showErrs :: (Show i) => String -> [i] -> IO o
showErrs msg errs =
  putStrLn ("There were " ++ msg ++ " in the input:") >>
  mapM_ print errs >> error "quitting now"

-- Add the default ccs to a CCSAmt that has a blank ccs

addDCCA :: Name -> CCSAmt -> CCSAmt
addDCCA dc c@(CCSAmt n a) = if n == noName then CCSAmt dc a else c

-- Initial validation of records: filter out comments, highlight parse
-- errors, and check that dates are valid. This is kind of a trivial
-- use of runLedger, it could almost be accomplished by partition.

validateRecs :: [Record] -> IO [Record]
validateRecs records =
  do let (_,i,e) = runLedger (chk records)
     unless (null e) (showErrs "parse or date errors" e)
     return i
  where chk [] = return ()
        chk (r@(ErrorRec _):rs) = recordErr r >> chk rs
        chk ((CommentRec _):rs) = chk rs
        chk (r:rs) =
            (if validDate (getRecDate r) then recordInfo else recordErr) r
            >> chk rs

-- Check that the default ccs is basic and that all derived ccs refer
-- to basic ccs; return the derived ccs with noName refers-to replaced
-- with explicit default ccs.

validateCCS :: Name -> [Record] -> [Record] -> IO [Record]
validateCCS dc cb cd =
  do let bn = map getRecName cb
         dn = map getRecName cd
     when (elem dc dn) (error (show dc ++ " is not a basic CCS!"))
     let (_,i,e) = runLedger (chk bn dn cd)
     unless (null e) (showErrs "problems with non-basic ccs" e)
     return i
  where chk _ _ [] = return ()
        chk bn dn (r@(CCSRec n d a nb):rs)
          | nb == noName    = recordInfo (CCSRec n d a dc) >> chk bn dn rs
          | elem nb bn      = recordInfo r >> chk bn dn rs
          | elem nb dn      = rE "non-basic" nb >> recordErr r >> chk bn dn rs
          | otherwise       = rE "unknown" nb   >> recordErr r >> chk bn dn rs
        chk _ _ (r:_) = intErr "validateCCS" r
        rE t n = recordErr (CommentRec (t ++ " ccs: " ++ show n))

-- Check that accounts with initial values refer to known ccs; replace
-- blank ccs-name with explicit default ccs

validateAccts :: Name -> [Record] -> [Record] -> IO [Record]
validateAccts dc ccs accts =
  do let cn = map getRecName ccs
     let (_,i,e) = runLedger (chk cn accts)
     unless (null e) (showErrs "problems with initial values in accounts" e)
     return i
  where chk _ [] = return ()
        chk cn (r@(AccountRec _ _ _ Nothing):rs) = recordInfo r >> chk cn rs
        chk cn (r@(AccountRec n da de (Just (CCSAmt nb ia))):rs)
          | nb == noName    =
            recordInfo (AccountRec n da de (Just (CCSAmt dc ia))) >> chk cn rs
          | elem nb cn      = recordInfo r >> chk cn rs
          | otherwise       = rE "unknown" nb >> recordErr r >> chk cn rs
        chk _ (r:_) = intErr "validateCCS" r
        rE t n = recordErr (CommentRec (t ++ " ccs: " ++ show n))

-- Classify records by type and sort accounts and
-- transaction-type records by date or name as needed

-- TODO: should really check that there are no duplications among
-- account names and account group names together, rather than
-- separately (or just account groups, right now)

classifyRecs :: [Record] -> (Name, [Record], [Record], [Record], [Record],
                             [Record], [Record], [Record], [Record], [Record])
classifyRecs rs = cw rs [] [] [] [] [] [] [] []
  where cw [] c i e a g t p r =
          let c1 = if null c
                      then [CCSRec (Name "zorkmid") "" Nothing noName]
                      else c
              dc = getRecName (last c1)
              (cb, cd) = partition isB (vsN c1)
          in (dc, cb, cd, vsN i, vsN e, reverse a, vsN g,
              asD dc t, asD dc r, asD dc p)
        cw (rec:recs) c i e a g t p r =
          case rec of
            CommentRec _       -> cw recs c i e a g t p r
            CCSRec _ _ _ _     -> cw recs (rec:c) i e a g t p r
            IncomeRec _ _      -> cw recs c (rec:i) e a g t p r
            ExpenseRec _ _     -> cw recs c i (rec:e) a g t p r
            AccountRec _ _ _ _ -> cw recs c i e (rec:a) g t p r
            GroupRec _ _       -> cw recs c i e a (rec:g) t p r
            PriceRec _ _ _ _   -> cw recs c i e a g t (rec:p) r
            RecurRec _ _ _ _   -> cw recs c i e a g t p (rec:r)
            _                  -> cw recs c i e a g (rec:t) p r
        vsN = uChk . sortBy cmpRecName
        uChk vs = if uniqAdjBy (\v1 v2 -> cmpRecName v1 v2 == EQ) vs
                     then vs
                     else error ("duplicate records\n  " ++
                                 intercalate "\n  " (map show vs))
        asD dc = sortBy cmpRecDate . reverse . map (addDC dc)
        addDC dc (PriceRec d imp ccsa1 ccsa2) =
          PriceRec d imp ccsa1 (addDCCA dc ccsa2)
        addDC dc (XferRec d f from tos m c) =
          XferRec d f from (map (addDCCAt dc) tos) m c
        addDC dc (ExchRec t d f acc ccsa1 ccsa2 m) =
          ExchRec t d f acc (addDCCA dc ccsa1) (addDCCA dc ccsa2) m
        addDC dc (RecurRec p dl dr r) = RecurRec p dl dr (addDC dc r)
        addDC _ r = r
        addDCCAt d (n,a) = (n, addDCCA d a)
        isB (CCSRec _ _ ma nb) = isNothing ma && nb == noName
        isB r = intErr "classifyCCS" r

-- Second validation of transactions: check that all from & to accounts
-- are valid, that splits aren't 1/0 or 0/1, etc

-- TODO: if we want to give a reason for each failure, run this
-- through runLedger as above? then we could generate multiple output
-- records for (some of) each input record... see validateCCS

validateTransPrices :: [Record] -> [Record] -> [Record] ->
                       [Record] -> [Record] -> IO ()
validateTransPrices ccs incs exps accts tps =
  do let bads = filter chk tps
     unless (null bads) (showErrs "bad transactions" bads)
  where chk (SplitRec _ c (Amount amt1) (Amount amt2)) =
            amt1 == 0 || amt2 == 0 || notIn c ccs
        chk (PriceRec _ _ (CCSAmt c1 amt1) (CCSAmt c2 _)) =
            amt1 == Amount 0 || notIn c1 ccs || notIn c2 ccs
        chk (XferRec _ _  from tos _ _) =
            (notIn from incs && notIn from accts) || any chkTo tos
        chk (ExchRec _ _ _ a (CCSAmt c1 _) (CCSAmt c2 _) _) =
            notIn a accts || notIn c1 ccs || notIn c2 ccs
        chk (NoteRec _ _ _ _) = False
        chk (RecurRec _ _ _ r) = chk r
        chk _ = True
        chkTo (to, CCSAmt n _) =
          (notIn to exps && notIn to accts) || notIn n ccs
        notIn _ [] = True
        notIn s (r:rs) = s /= getRecName r && notIn s rs

-- Generate implicit price/date information from buy and sell transactions;
-- presumably, these will have taken place at market price, which is what we
-- want. Only generate info for transactions involving the default currency,
-- other stuff is too hard to untangle at least for now.

generateImplicitPrices :: Name -> [Record] -> [Record] -> [Record]
generateImplicitPrices dc trs cd = gip (geni cd) trs
  where geni = map gii . filter iG
        iG (CCSRec _ _ ma _) = isJust ma
        iG r = intErr "generateImplicitPrices:1" r
        gii (CCSRec n _ (Just a) nb) =
          PriceRec startTime True (CCSAmt n (Amount 1))
                   (addDCCA dc (CCSAmt nb a))
        gii r = intErr "generateImplicitPrices:2" r
        gip acc [] = filter pr acc
        gip acc (t@(ExchRec _ _ _ _ _ _ _):ts) = gip (genp t : acc) ts
        gip acc (_:ts) = gip acc ts
        genp (ExchRec _ date _ _ (CCSAmt n1 a1) (CCSAmt n2 a2) _)
          | n1 == dc   = PriceRec date True (nC n2 a2 a2) (nC n1 a1 a2)
          | n2 == dc   = PriceRec date True (nC n1 a1 a1) (nC n2 a2 a1)
          | otherwise  = CommentRec "general exchange, no price generated"
        genp r = intErr "generateImplicitPrices:3" r
        pr (PriceRec _ _ _ _) = True
        pr _ = False
        nC n (Amount a1) (Amount a2) =
          CCSAmt n (Amount (if a2 == 0 then a1 else roundP 4 (a1/a2)))

getCN :: CCSAmt -> Name
getCN (CCSAmt n _) = n

getCA :: CCSAmt -> Rational
getCA (CCSAmt _ (Amount a)) = a

addTo :: [CCSAmt] -> CCSAmt -> [CCSAmt]
addTo [] n = [n]
addTo (q:qs) d =
  let qn = getCN q
      qq = getCA q
      dn = getCN d
      dq = getCA d
      nq = qq + dq
  in if qn == dn
        then if nq == 0 then qs else CCSAmt qn (Amount nq) : qs
        else q : addTo qs d

subFrom :: [CCSAmt] -> CCSAmt -> [CCSAmt]
subFrom acc d = addTo acc (CCSAmt (getCN d) (Amount (-(getCA d))))

scaleBy :: [CCSAmt] -> CCSAmt -> [CCSAmt]
scaleBy qs d = map (s1 (getCN d) (getCA d)) qs
  where s1 dn dq q =
          let qn = getCN q
              qq = getCA q
          in if qn == dn then CCSAmt qn (Amount (qq * dq)) else q

maybeRecord :: Maybe Name -> Record -> AccountData -> (Name -> Bool) ->
               Ledger e (Record, [CCSAmt]) ()
maybeRecord reg record newaccs tst =
  let isJ = isJust reg
      rn = fromJust reg
      acc = filter (\a -> fst a == rn) newaccs
      nb = if null acc
              then [CCSAmt noName (Amount 0)]
              else snd (head acc)
  in if isJ && tst rn
        then recordInfo (record, nb)
        else recordNil

maybeDo :: Maybe Name -> Bool -> Record -> Bool ->
           AccountData -> AccountData -> (Name -> Bool) ->
           Ledger e (Record, [CCSAmt]) AccountData
maybeDo reg dorec record isrec accs newaccs tst =
  if dorec
     then if isrec
             then return newaccs
             else maybeRecord reg record newaccs tst >> return accs
     else maybeRecord reg record newaccs tst >> return newaccs

exchTrans :: Maybe Name -> Bool -> Record -> AccountData ->
             Ledger e (Record, [CCSAmt]) AccountData
exchTrans reg dorec record@(ExchRec _ _ isrec acc amtn amto _) accs =
  maybeDo reg dorec record isrec accs (doExch accs acc amtn amto)
          (\rn -> rn == acc || rn == noName)
  where doExch [] _ _ _ = []
        doExch ((an,ab):as) n en eo =
          if an == n
             then (an, subFrom (addTo ab en) eo) : as
             else (an,ab) : doExch as n en eo
exchTrans _ _ r _ = intErr "exchTrans" r

xferTrans :: Maybe Name -> Bool -> Record -> AccountData ->
             Ledger e (Record, [CCSAmt]) AccountData
xferTrans reg dorec record@(XferRec _ isrec from tos _ _) accs =
  foldM (xfer1 False) accs (init tos) >>= (\a -> xfer1 True a (last tos))
  where xfer1 rf as (to,amt) =
          maybeDo reg dorec record isrec as (doXfer as from to amt)
                  (\rn -> (rf && (rn == from || rn == noName)) || rn == to)
        doXfer [] _ _ _ = []
        doXfer (a@(an,ab):as) nf nt e
          | an == nf    = (an, subFrom ab e) : doXfer as nf nt e
          | an == nt    = (an, addTo ab e) : doXfer as nf nt e
          | otherwise   = a : doXfer as nf nt e
xferTrans _ _ r _ = intErr "xferTrans" r

-- TODO: look into account and see if split is applicable? yeah... cleaner
-- Also: does maybeDo apply here, too? I think probably it should...
-- but is this a reconcilable transaction? It reaches across accounts,
-- so maybe not

splitTrans :: Maybe Name -> Record -> AccountData ->
              Ledger e (Record, [CCSAmt]) AccountData
splitTrans reg record@(SplitRec _ ccs (Amount an) (Amount ao)) acc =
  let newaccs = map doST acc
  in maybeRecord reg record newaccs (const True) >> return newaccs
  where doST (a1,a2) = (a1, scaleBy a2 (CCSAmt ccs (Amount (an/ao))))
splitTrans _ r _ = intErr "splitTrans" r

{-
-- new version with printing of initial values
mkInit reg as =
  let iaccs = (map (\a -> (getRecName a, maybeToList (gI a))) as)
  in maybeRecord reg (CommentRec "") iaccs (const True) >> return iaccs
  where gI (AccountRec _ _ _ mi) = mi
        gI r = intErr "mkInit" r
-}

mkInit :: Monad m => [Record] -> m AccountData
mkInit as = return (map (\a -> (getRecName a, maybeToList (gI a))) as)
  where gI (AccountRec _ _ _ mi) = mi
        gI r = intErr "mkInit" r

appTr :: Date -> Maybe Name -> Bool -> [Record] -> AccountData ->
         Ledger Record (Record, [CCSAmt]) AccountData
appTr _ _ _ [] as = return as
appTr d r f (t:ts) as =
  if getRecDate t > d
     then return as
     else case t of
            XferRec _ _ _ _ _ _ -> xferTrans r f t as >>= appTr d r f ts
            ExchRec _ _ _ _ _ _ _ -> exchTrans r f t as >>= appTr d r f ts
            SplitRec _ _ _ _  -> splitTrans r t as >>= appTr d r f ts
            NoteRec _ isrec SN_T _ ->
              (if isrec then recordNil else recordInfo (t,[]))
                >> appTr d r f ts as
            NoteRec da isrec _ _ ->
              (if isrec || not (ca d da)
                  then recordNil
                  else recordInfo (t,[])) >> appTr d r f ts as
            _ -> recordErr t >> appTr d r f ts as
  where ca dn@(Date yn _ _) (Date _ ma da) =
          let jn = julianDate dn
              j1 = julianDate (Date (yn - 1) ma da)
              j2 = julianDate (Date yn ma da)
              j3 = julianDate (Date (yn + 1) ma da)
          in abs (jn - j1) <= 7 || abs (jn - j2) <= 7 || abs (jn - j3) <= 7

showT :: (Record, [CCSAmt]) -> IO ()
showT (t,_) = print t

showTB :: (Record, [CCSAmt]) -> IO ()
showTB e@(_,b) = showT e >> mapM_ sB b
  where sB ccsa = putStrLn ('\t' : show ccsa)

getBalances :: Date -> Date -> Maybe Name -> Bool ->
               [Record] -> [Record] -> IO AccountData
getBalances date1 date2 reg dorec accts trans =
  do let (r,i1,e) = runLedger (mkInit accts >>= appTr date2 reg dorec trans)
         i = dropWhile (\t -> getRecDate (fst t) < date1) i1
         ss = if dorec then showT else showTB
     unless (null e) (showErrs "processing errors" e)
     unless (null i) (putStrLn "Notes:" >> mapM_ ss i >> putStrLn "")
     return r

-- For now, we don't generate "swap prices" internally, so unless the user
-- enters some, we won't see any; see also generateImplicitPrices above.

getPrices :: Name -> Name -> Date -> Date -> [Record] -> IO ()
getPrices nm dc date1 date2 prices =
  do let p1 = dropWhile (\t -> date2 < getRecDate t) prices
         p2 = takeWhile (\t -> date1 < getRecDate t) p1
         (_,i,e) = runLedger (get p2)
     unless (null e) (doShow "Swap \"Prices\"" e >> putStrLn "")
     unless (null i) (doShow "Ordinary Prices" i)
     when (null i && null e)
          (putStrLn ("No prices known for " ++ show nm))
  where get [] = return ()
        get (p@(PriceRec _ _ (CCSAmt nr1 _) (CCSAmt nr2 _)):ps) =
          if (nr1 == nm && nr2 == dc) || (nr1 == dc && nr2 == nm)
             then recordInfo p >> get ps
             else if nr1 == nm || nr2 == nm
                     then recordErr p >> get ps
                     else get ps
        get _ = recordNil
        doShow t p = putStrLn t >> mapM_ print (reverse p)

-- Convert a list of RecurRec records into equivalent list
-- of individual transactions, sorted by date

expandRecurringTrans :: [Record] -> [Record]
expandRecurringTrans rs = sortBy cmpRecDate (concatMap eRT rs)
  where eRT (RecurRec (PND n) dl dr rec) =
          map (mRD rec dr) (genD (getRecDate rec) n dl)
        eRT (RecurRec PSW dl dr rec) =
          let da = getRecDate rec
              db = offsetDate da 3
              mf = map (mRD rec dr)
          in mf (genD da 7 dl) ++ mf (genD db 7 dl)
        eRT (RecurRec (PNM n) dl dr rec) =
          map (mRD rec dr) (genM (getRecDate rec) n dl)
        eRT (RecurRec PSM dl dr rec) =
          let da = getRecDate rec
              db = offsetDate da 15
              mf = map (mRD rec dr)
          in mf (genM da 1 dl) ++ mf (genM db 1 dl)
        eRT rec = intErr "expandRecurringTrans" rec
        genD d1 dd d2 =
          let j = julianDate d1
          in map gregorianDate [j, j + dd .. julianDate d2]
        genM d1 dm d2 = if d1 <= d2 then d1 : genM (oMo d1 dm) dm d2 else []
        oMo (Date y m d) mstep =
          let (dy,m1) = divMod (mstep + m - 1) 12
          in Date (y + dy) (m1 + 1) d
        mRD (XferRec _ _ f t m i) dr dc =
          XferRec dc (dc <= dr) f t m i
        mRD (ExchRec t _ _ a c1 c2 m) dr dc =
          ExchRec t dc (dc <= dr) a c1 c2 m
        mRD r _ _ = intErr "expandRecurringTrans" r
