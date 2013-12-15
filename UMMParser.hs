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

$Id: UMMParser.hs,v 1.52 2010/07/27 04:09:05 uwe Exp $ -}

-- TODO: template := <to be determined>

module UMMParser (parseURecord, parseUDate, parseUCommand) where
import Prelude
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec as TPCP hiding (spaces)

import UMMData

intErr :: String -> o
intErr loc = error ("internal error at " ++ loc ++ ", please report this")

readInt :: String -> Integer
readInt s = foldl ma 0 s
  where ma v1 v2 = 10*v1 + toInteger (digitToInt v2)

readAmt :: String -> String -> Rational
readAmt ip fp =
  let l = length fp
      e = 10^l
      iv = readInt ip
      fv = readInt fp
  in (e*iv + fv) % e

-- Parse a string, recognizing the whole string even if only a partial
-- prefix of it is given: the size of the minimal acceptable prefix has
-- to be specified to the parser; and if the minimal acceptable prefix
-- isn't unique, bad things happen

parsePrefixOf :: Int -> String -> Parser String
parsePrefixOf n str =
  string (take n str) >> opts (drop n str) >> return str
  where opts [] = return ()
        opts (c:cs) = optional (char c >> opts cs)

-- Hide parsec's spaces and substitute this one:
-- want "spaces" to mean an actual something there

spaces, parseString, parseOptionalString, parseXferID :: Parser String
escChar, currencySymbol, otherSymbol :: Parser Char

spaces = many1 space

escChar =
  do char '\\'
     c <- anyChar
     return (case c of
             'a' -> chr 7
             'b' -> chr 8
             't' -> '\t'
             'n' -> '\n'
             'v' -> chr 11
             'f' -> chr 12
             'r' -> '\r'
             _ -> c)

-- The usual quoted-string with escaped quotes inside

parseString =
  do spaces
     char '"'
     str <- many (escChar <|> noneOf "\"")
     char '"'
     return str

parseOptionalString = option "" (TPCP.try parseString)

-- TODO: are there some here that we need to add or remove?

-- Currency symbols: do allow these as the first char of a name
-- gathered from various places on the web, mainly unicode.org

currencySymbol =
  oneOf ['\x24',	-- dollar
         '\xa2',	-- cent
         '\xa3',	-- pound
         '\xa4',	-- generic currency symbol
         '\xa5',	-- yen
         '\x192',	-- florin
         '\x9f2',	-- Bengali rupee 1
         '\x9f3',	-- Bengali rupee 2
         '\xaf1',	-- gujarati rupee
         '\xbf9',	-- Tamil rupee
         '\xe2f',	-- Thai baht
         '\x17db',	-- Khmer currency symbol
         '\x20a0',	-- euro-currency (not euro)
         '\x20a1',	-- colon
         '\x20a2',	-- Brazilian cruzeiro
         '\x20a3',	-- French franc
         '\x20a4',	-- Italian lira
         '\x20a5',	-- mill
         '\x20a6',	-- Nigerian naira
         '\x20a7',	-- Spanish peseta
         '\x20a8',	-- Indian rupee
         '\x20a9',	-- Korean won
         '\x20aa',	-- Israeli new sheqel
         '\x20ab',	-- Vietnamese dong
         '\x20ac',	-- European euro
         '\x20ad',	-- Laotian kip
         '\x20ae',	-- Mongolian tugrik
         '\x20af',	-- Greek drachma
         '\x20b0',	-- German pfennig
         '\x20b1',	-- Phillipine peso
         '\x20b2',	-- Paraguayan guarani ???
         '\x20b3',	-- austral ???
         '\x20b4',	-- Ukrainian hryvnia ???
         '\x20b5',	-- Ghanaian cedi ???
         '\x20b6',	-- livre tournois ???
         '\x20b7',	-- spesmilo ???
         '\x20b8',	-- tenge ???
         '\x3350',	-- yuan 1
         '\x5143',	-- Chinese yuan
         '\x5186',	-- yen 2
         '\x5706',	-- yen/yuan variant
         '\x570e',	-- yen/yuan variant
         '\x5713',	-- yuan variant
         '\x571c',	-- yuan variant
         '\xa838',	-- North Indic rupee ???
         '\xfdfc']	-- Iranian rial

-- Non-currency symbols: don't allow these as the first character of a name

otherSymbol = oneOf "!%&*+-/:<=>?@^_~"

parseName :: Parser Name
parseName =
  do many space
     first <- letter <|> currencySymbol
     rest <- many (letter <|> currencySymbol <|> digit <|> otherSymbol)
     return (Name (first:rest))

parseOptionalName :: Parser Name
parseOptionalName = option noName (TPCP.try parseName)

parseInt :: Parser Integer
parseInt = many1 digit >>= return . readInt

parseXferID = spaces >> many1 (digit <|> letter <|> oneOf "-/_@")

-- Parse first alternative for amounts: \d+(\.\d*)?

parseAmt1, parseAmt2, parseAmt3 :: Parser Rational
parseAmt1 =
  many1 digit >>=
  (\ip -> option "" (char '.' >> many digit) >>= return . readAmt ip)

-- Parse second alternative for amounts: \.\d+

parseAmt2 = char '.' >> many1 digit >>= return . readAmt "0"

-- Parse third alternative for amounts: \d+/\d+: this is how rationals
-- get displayed if they're not integers scaled by a power of 10, so
-- we'd better be able to handle them...

parseAmt3 = parseInt >>= (\n -> char '/' >> parseInt >>= (\d -> return (n%d)))

-- Parse a floating-point-formatted number as either \d+(\.\d*)? or \.\d+

parseAmount, parseOptionalAmount :: Parser Amount

parseAmount =
  do many space
     s <- option '+' (oneOf "+-")
     v <- TPCP.try parseAmt3 <|> parseAmt1 <|> parseAmt2
     return (Amount (if s == '+' then v else negate v))

parseOptionalAmount = option (Amount 1) (TPCP.try parseAmount)

parseCCSAmt :: Parser CCSAmt
parseCCSAmt =
  do amt <- parseAmount
     ccs <- parseOptionalName
     return (CCSAmt ccs amt)

parseOneTo, parseManyTo :: Parser [(Name, CCSAmt)]

parseOneTo =
  do to <- parseName
     ca <- parseCCSAmt
     return [(to, ca)]

parseManyTo =
  do spaces
     char '{'
     tos <- sepBy1 (parseOneTo >>= (\t -> many space >> return t)) (char ',')
     char '}'
     return (concat tos)

-- A date: three groups of digits separated by '/' or '-' (but not mixed)
-- where the first group of digits is the year, the second the month, and
-- the third the day: YYYY/MM/DD or YYYY-MM-DD: no messing around with
-- whether days come first or months, and also no implicit 19xx or 20xx
-- or whatever.

parseDate :: Parser Date
parseDate =
  do spaces
     y <- parseInt
     m <- pMS '/' <|> pMS '-'
     d <- parseInt
     return (Date (fromInteger y) (fromInteger m) (fromInteger d))
  where pMS s = char s >> parseInt >>= (\m -> char s >> return m)

-- Parse a date range in any of several formats:
--   DR1 -> an optional pair of dates, with defaults
--   DR2 -> a year/month, generating a fractional-month-long range
--   DR3 -> a year, generating a fractional-year-long range
--   DR4 -> literal 'last' followed by a number -> that many days into past
-- In DR[23], if the fractional input date is the same as the current
-- date, go from the beginning of the period to the current date;
-- otherwise, go the full period.

parseDateRange, parseDR1, parseDR2, parseDR3, parseDR4 ::
  Date -> Parser (Date, Date)
parseDR1 now =
  do date1 <- option now parseDate
     date2 <- option startTime parseDate
     return (if date2 < date1 then (date2, date1) else (date1, date2))

parseDR2 dn@(Date yn mn _) =
  do spaces
     y <- parseInt
     oneOf "/-"
     m <- parseInt
     let yi = fromInteger y
         mi = fromInteger m
         d1 = Date yi mi 1
         d2 = if yi == yn && mi == mn
                 then dn
                 else previousDate (Date yi (mi + 1) 1)
     return (d1, d2)

parseDR3 dn@(Date yn _ _) =
  do spaces
     y <- parseInt
     let yi = fromInteger y
         d1 = Date yi 1 1
         d2 = if yi == yn then dn else Date yi 12 31
     return (d1, d2)

parseDR4 dn =
  do spaces
     string "last"
     spaces
     dd <- parseInt
     return (offsetDate dn (- fromInteger dd), dn)

parseDateRange now =
  TPCP.try (parseDR1 now) <|>
  TPCP.try (parseDR2 now) <|>
  TPCP.try (parseDR3 now) <|>
  parseDR4 now

parseReconcile :: Parser Bool
parseReconcile =
  option False (TPCP.try (many space >> oneOf "*!" >> return True))

parsePeriod :: Parser Period
parsePeriod =
  spaces >> (pPG <|>
             pPS "daily" (PND 1) <|>
             pPS "weekly" (PND 7) <|>
             pPS "monthly" (PNM 1) <|>
             pPS "quarterly" (PNM 3) <|>
             pPS "annually" (PNM 12) <|>
             TPCP.try (pPS "biweekly" (PND 14)) <|>
             TPCP.try (pPS "bimonthly" (PNM 2)) <|>
             pPS "biannually" (PNM 24) <|>
             TPCP.try (pPS "semiweekly" PSW) <|>
             TPCP.try (pPS "semimonthly" PSM) <|>
             pPS "semiannually" (PNM 6))
  where pPS s p = string s >> return p
        pPG = do n <- parseInt
                 spaces
                 p <- parsePrefixOf 1 "days" <|>
                      parsePrefixOf 1 "weeks" <|>
                      parsePrefixOf 1 "months" <|>
                      parsePrefixOf 1 "years"
                 let ni = fromInteger n
                 return (case p of
                         "days" -> PND ni
                         "weeks" -> PND (7*ni)
                         "months" -> PNM ni
                         "years" -> PNM (12*ni)
                         _ -> intErr "parsePeriod")

-- The top-level record parsers

parseCCS, parseIE, parseAccount, parseGroup, parsePrice, parseXfer, parseEBS,
  parseSplit, parseTBA, parseRecur, parseComment, parseBlank, parseRecord ::
  Parser Record

parseCCS =
  do string "ccs"
     name <- parseName
     desc <- parseOptionalString
     ma <- option Nothing (TPCP.try (parseAmount >>= return . Just))
     bname <- parseOptionalName
     return (CCSRec name desc ma bname)

parseIE =
  do rtype <- string "income" <|> string "expense"
     name <- parseName
     desc <- parseOptionalString
     return (case rtype of
               "income" -> IncomeRec name desc
               "expense" -> ExpenseRec name desc
               _ -> intErr "parseIE")

parseAccount =
  do string "account"
     rec <- parseReconcile
     name <- parseName
     date <- option startTime (TPCP.try parseDate)
     desc <- parseOptionalString
     ival <- option Nothing (TPCP.try (parseCCSAmt >>= return . Just))
     return (AccountRec name date rec desc ival)

parseGroup =
  do string "group"
     names <- many1 parseName
     return (GroupRec (head names) (tail names))

parsePrice =
  do string "price"
     date <- parseDate
     amt1 <- parseOptionalAmount
     name1 <- parseName
     ca2 <- parseCCSAmt
     return (PriceRec date False (CCSAmt name1 amt1) ca2)

parseXfer =
  do string "xfer"
     rec <- parseReconcile
     date <- parseDate
     from <- parseName
     to <- TPCP.try parseOneTo <|> parseManyTo
     memo <- parseOptionalString
     ident <- option "" (TPCP.try parseXferID)
     return (XferRec date rec from to memo ident)

-- 'buy' and 'sell' are purely syntactic sugar for 'exch': 'buy' is
-- exactly the same, and 'sell' is the same as if {amt1,ccs1} and
-- {amt2,ccs2} were swapped in an exch

parseEBS =
  do rtype <- string "buy" <|> string "sell" <|> string "exch"
     rec <- parseReconcile
     date <- parseDate
     acct <- parseName
     amt1 <- parseAmount
     name1 <- parseName
     ca2 <- parseCCSAmt
     memo <- parseOptionalString
     let et = case rtype of
                "buy" -> BSE_B
                "sell" -> BSE_S
                "exch" -> BSE_E
                _ -> intErr "parseEBS"
         ca1 = CCSAmt name1 amt1
     if et == BSE_S
        then return (ExchRec et date rec acct ca2 ca1 memo)
        else return (ExchRec et date rec acct ca1 ca2 memo)

parseSplit =
  do string "split"
     date <- parseDate
     name <- parseName
     amt1 <- parseAmount
     amt2 <- parseAmount
     return (SplitRec date name amt1 amt2)

parseTBA =
  do rtype <- string "todo" <|> string "birthday" <|> string "anniversary"
     rec <- parseReconcile
     date <- parseDate
     spaces
     memo <- many anyChar
     let nt = case rtype of
                "todo" -> SN_T
                "birthday" -> SN_B
                "anniversary" -> SN_A
                _ -> intErr "parseTBA"
     return (NoteRec date rec nt (trimspace memo))

parseComment =
  do many1 (oneOf "#;")
     many space
     comment <- many anyChar
     return (CommentRec (trimspace comment))

parseRecur =
  do string "recurring"
     period <- parsePeriod
     spaces			-- TODO: keep this syntactic sugar?
     string "until"		-- it kinda reads better...?
     dl <- parseDate
     spaces
     dr <- option startTime
                  (TPCP.try (parsePrefixOf 3 "reconciled" >> parseDate))
     many space
     record <- TPCP.try parseEBS <|> parseXfer <|> parseTBA
     return (RecurRec period dl dr record)

parseBlank = many space >> return (CommentRec "")

parseRecord =
  do many space
     record <- parsePrice
           <|> parseXfer
           <|> parseCCS
           <|> TPCP.try parseIE
           <|> TPCP.try parseEBS
           <|> TPCP.try parseAccount
           <|> parseSplit
           <|> parseTBA
           <|> parseGroup
           <|> parseRecur
           <|> parseComment
           <|> parseBlank	-- this must be last, as it can match nothing
     many space >> eof >> return record

parseURecord :: String -> Record
parseURecord input =
  case parse parseRecord "umm record" input of
       Left _ -> ErrorRec input
       Right val -> val

parseUDate :: String -> Either ParseError Date
parseUDate input = parse parseDate "umm date" (' ' : input)

parseCmdBalance, parseCmdBasis, parseCmdChange, parseCmdPlot, parseCmdPrice,
  parseCmdReconcile, parseCmdRegister, parseCmdToDo, parseCommand ::
  Date -> Parser Command
parseCmdExport, parseCmdList :: Parser Command

parseCmdBalance now =
  do parsePrefixOf 3 "balance"
     name <- parseOptionalName
     date <- option now parseDate
     return (BalanceCmd name date)

parseCmdBasis now =
  do parsePrefixOf 3 "basis"
     name <- parseName
     date <- option now parseDate
     return (BasisCmd name date)

parseCmdExport = parsePrefixOf 1 "export" >> return ExportCmd

-- TODO: make verbose/nonverbose work... somehow add optional verbosity

parseCmdChange now =
  do parsePrefixOf 1 "change"
     name <- parseName
     (date1, date2) <- parseDateRange now
     return (ChangeCmd False name date1 date2)

parseCmdList =
  do parsePrefixOf 1 "list"
     many space
     w <- option "all" (parsePrefixOf 1 "ccs" <|>
                        parsePrefixOf 1 "groups" <|>
                        parsePrefixOf 1 "incomes" <|>
                        parsePrefixOf 1 "expenses" <|>
                        TPCP.try (parsePrefixOf 2 "all") <|>
                        parsePrefixOf 2 "accounts")
     return (ListDataCmd (case w of
                            "all" -> COLAll
                            "ccs" -> COLCCS
                            "accounts" -> COLAccs
                            "groups" -> COLGrps
                            "incomes" -> COLIncs
                            "expenses" -> COLExps
                            _ -> intErr "parseListCmd"))

parseCmdPlot now =
  do parsePrefixOf 2 "plot"
     name <- parseName
     (date1, date2) <- parseDateRange now
     output <- option (Name "umm_plot") parseName
     return (PlotCmd name date1 date2 output)

parseCmdPrice now =
  do parsePrefixOf 2 "price"
     name <- parseName
     (date1, date2) <- parseDateRange now
     return (PriceCmd name date1 date2)

parseCmdReconcile now =
  do parsePrefixOf 3 "reconcile"
     name <- parseOptionalName
     date <- option now parseDate
     return (ReconcileCmd name date)

parseCmdRegister now =
  do parsePrefixOf 3 "register"
     name <- parseName
     (date1, date2) <- parseDateRange now
     return (RegisterCmd name date1 date2)

parseCmdToDo now =
  do parsePrefixOf 1 "todo"
     date <- option now parseDate
     return (ToDoCmd date)

parseCommand date =
  do cmd <- parseCmdChange date
        <|> parseCmdExport
        <|> parseCmdList
        <|> TPCP.try (parseCmdPlot date)
        <|> parseCmdPrice date
        <|> parseCmdToDo date
        <|> TPCP.try (parseCmdBalance date)
        <|> parseCmdBasis date
        <|> TPCP.try (parseCmdRegister date)
        <|> parseCmdReconcile date
     eof
     return cmd

parseUCommand :: Date -> String -> Command
parseUCommand now input =
  case parse (parseCommand now) "umm command" input of
       Left err -> error (show err)
       Right val -> val
