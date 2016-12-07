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

$Id: help-head.txt,v 1.2 2010/05/10 04:33:16 uwe Exp $ -}

module UMMHelp (writeHdr, usageMsg) where
import Prelude

version :: String
version = "0.2.1"

writeHdr :: String
writeHdr =
  "# Uwe's Money Manager -- umm " ++ version ++
  "\n# Copyright 2009-2010 Uwe Hollerbach <uh@alumni.caltech.edu>\n" ++
  "# Available under GPL V3 or later. Share and enjoy!\n" ++
  "# http://www.korgwal.com/umm/\n"

usageMsg :: String -> String
usageMsg prog =
  "usage is '" ++ prog ++ " <ledger-file> <command> <options>'\n" ++
  "where <command> and <options> can be any of the following:\n" ++
  "\n" ++
  "    'balance' [account-or-group] [date]\n" ++
  "    'change' acc-or-inc-or-exp [date-range]\n" ++
  "    'list' ['all' | 'accounts' | 'ccs' | 'expenses' | 'incomes' | 'groups']\n" ++
  "    'price' ccs [date-range]\n" ++
  "    'register' account [date-range]\n" ++
  "    'reconcile' [account] [date]\n" ++
  "    'todo' [date]\n" ++
  "    'basis' ccs [date]\n" ++
  "    'export'\n" ++
  "\n" ++
  "The commands may be shortened to the unique prefixes or anything\n" ++
  "longer: 'b', 'ba', 'bala' etc, 'reg', 'regi' etc, but not 'r'. If no\n" ++
  "command or options are specified, the default action is to show the\n" ++
  "balances of all accounts as of the current date.\n" ++
  "\n" ++
  "'account' is the name of an account; see the description of 'name'\n" ++
  "below for more details of formatting. Likewise, 'account-or-group'\n" ++
  "is the name of an account or an account group, and 'acc-or-inc-or-exp'\n" ++
  "is the name of an account or an income or expense pseudo-account.\n" ++
  "\n" ++
  "'ccs' is the name of a currency, commodity, or security; again, see\n" ++
  "the description of 'name' below for more details of formatting.\n" ++
  "\n" ++
  "'date' defaults to the current date if not specified; again, see\n" ++
  "below for details of formatting.\n" ++
  "\n" ++
  "'date-range' may be specified in any of several formats; see below\n" ++
  "for details of formatting.\n" ++
  "\n" ++
  "--------\n" ++
  "\n" ++
  "* 'balance' shows the balance in the specified account or group,\n" ++
  "  or in all accounts if none is specified, as of the given date.\n" ++
  "\n" ++
  "* 'change' shows the change in the specified account or pseudo-account\n" ++
  "  in the given date range.\n" ++
  "\n" ++
  "* 'price' shows the price history of the specified currency,\n" ++
  "  commodity, or security, in the given date range\n" ++
  "\n" ++
  "* 'register' shows all transactions involving the specified\n" ++
  "  account in the given date range, and shows the balance as of\n" ++
  "  the end of that date range.\n" ++
  "\n" ++
  "* 'reconcile' applies all reconciled transactions up to the\n" ++
  "  given date, shows relevant unreconciled transactions up to\n" ++
  "  that date, and shows the reconciled balance(s) as of that\n" ++
  "  date.\n" ++
  "\n" ++
  "  If 'reconcile' is given an account, then only unreconciled\n" ++
  "  transactions involving that account are shown, otherwise all\n" ++
  "  unreconciled transactions are shown\n" ++
  "\n" ++
  "* 'todo' shows all the unreconciled 'todo' items in the ledger\n" ++
  "  up to the date specified, defaulting to the current date\n" ++
  "\n" ++
  "* 'list' shows summaries of the various kinds of non-transaction\n" ++
  "  entries in the ledger file: currencies/commodities/securities,\n" ++
  "  income and expense categories, accounts, and groups\n" ++
  "\n" ++
  "* 'basis' shows the cost basis for a given currency or commodity\n" ++
  "  or security.\n" ++
  "\n" ++
  "* 'export' will eventually export the data in ledger/hledger\n" ++
  "  format. It is only partially implemented.\n" ++
  "\n" ++
  "--------\n" ++
  "\n" ++
  "There are several kinds of records in a ledger file:\n" ++
  "\n" ++
  "    'ccs' name [desc] [amount] [name]\n" ++
  "    'income' name [desc]\n" ++
  "    'expense' name [desc]\n" ++
  "    'account' name [date] [desc]\n" ++
  "    'group' name [name...]\n" ++
  "    'price' date [amount1] name1 amount2 [name2]\n" ++
  "    'split' date name amount1 amount2\n" ++
  "    'todo' [rec] date text\n" ++
  "    [period] 'xfer' [rec] date name1 name2 amount [name] [desc] [id]\n" ++
  "    [period] 'xfer' [rec] date name1 {name2 amount [name] [desc],\\\n" ++
  "        \\ name3 amount [name] [desc], ...} [desc] [id]\n" ++
  "    [period] 'exch' [rec] date name amount1 name1 amount2 [name2] [desc]\n" ++
  "\n" ++
  "There are also 'buy' and 'sell' records which are just syntactic sugar\n" ++
  "for 'exch', and 'birthday' and 'anniversary' records which are mostly\n" ++
  "just syntactic sugar for 'todo'; see more details below in the section\n" ++
  "describing the meaning of each type of record. The individual fields\n" ++
  "for each record are separated by whitespace.\n" ++
  "\n" ++
  "Blank lines or lines whose first non-blank character is '#' or ';' are\n" ++
  "treated as comments and are ignored.\n" ++
  "\n" ++
  "Syntactically, all of these are optional: there are no required\n" ++
  "records, and an empty ledger file is syntactically legal. However, a\n" ++
  "minimally-useful ledger file will probably contain at least some\n" ++
  "'xfer' records, which in turn require that there be at least a couple\n" ++
  "of 'account' or 'income' or 'expense' records.\n" ++
  "\n" ++
  "The order of records in the ledger file is not significant; the\n" ++
  "program orders them by type and date, and applies transactions in\n" ++
  "order by date.\n" ++
  "\n" ++
  "The second form of the 'xfer' record above shows splitting of records\n" ++
  "across multiple physical lines. All whitespace between the trailing\n" ++
  "'\\' on the first line and the leading '\\' on the second line, plus the\n" ++
  "two '\\', is annihilated and the remainders are joined.\n" ++
  "\n" ++
  "The meaning of each record type is as follows:\n" ++
  "\n" ++
  "* 'ccs name [desc] [amount] [name]' describes a currency or commodity\n" ++
  "  or security: the things you want to keep track of. The first ccs\n" ++
  "  record in the ledger file is the default unit; my ledger file has\n" ++
  "  'ccs US$' very near the top. However, you don't need one, in which\n" ++
  "  case the program computes in Zorkmid ('zm'). If you don't enter any\n" ++
  "  'ccs' record, you can't specify any units in 'xfer' records, and you\n" ++
  "  can't use any 'price' or 'split' records. You can use 'exch' (and\n" ++
  "  'buy' and 'sell') records, but they probably won't be very useful,\n" ++
  "  since all you can do is to trade one amount of Zorkmids for\n" ++
  "  another. The 'desc' field in a ccs record is currently just for\n" ++
  "  documentation, it's not used by the program. The optional 'amount'\n" ++
  "  field is for specifying the initial price of this ccs in units of\n" ++
  "  the second 'name' ccs, or the default ccs if no second 'name' is\n" ++
  "  specified. If present, the second 'name' is also the ccs into which\n" ++
  "  this ccs is translated.\n" ++
  "\n" ++
  "* 'account name [date] [desc]' records specify accounts where you\n" ++
  "  want to keep of the quantity of what's in the account as well as\n" ++
  "  transactions which move stuff into or out of the account. An\n" ++
  "  account can contain multiple types of ccs, for example a single\n" ++
  "  account could be used to describe a brokerage account containing\n" ++
  "  many securities as well as cash. The date and desc fields are\n" ++
  "  currently just for documentation and are not used by the program.\n" ++
  "\n" ++
  "* 'group name [name...]' groups multiple accounts together into one\n" ++
  "  group, so that it's possible to query the balances for a group of\n" ++
  "  accounts as a whole.\n" ++
  "\n" ++
  "* 'income name [desc]' and 'expense name [desc]' records specify\n" ++
  "  pseudo-accounts where you don't want to keep track of what's inside,\n" ++
  "  just the transactions: think of these as categories. 'income'\n" ++
  "  specifies a source of ccs, and 'expense' specifies a destination or\n" ++
  "  sink. The desc field is currently just for documentation. You can,\n" ++
  "  to some extent, circumvent the source or sink nature by entering a\n" ++
  "  transaction from a source to an account, but with a negative amount:\n" ++
  "  this would actually take money out of the account. This is intended\n" ++
  "  for VERY OCCASIONAL overrides, say you overpaid a utility bill and\n" ++
  "  get a credit which you don't want to just apply to the next\n" ++
  "  bill. Don't abuse this!\n" ++
  "\n" ++
  "* 'price date [amount1] name1 amount2 [name2]' records are used to\n" ++
  "  specify the price of one ccs in terms of another; usually a\n" ++
  "  currency. If the first amount is not specified, it defaults to 1,\n" ++
  "  and if the second name is not specified, it defaults to the default\n" ++
  "  ccs. For example, if you are tracking ounces of gold using the ccs\n" ++
  "  name 'Au', you might have a price record\n" ++
  "\n" ++
  "        price 2009-10-21 Au 1063.70\n" ++
  "\n" ++
  "  and if you track ounces but for some reason have a price quote in\n" ++
  "  grams, you might write\n" ++
  "\n" ++
  "        price 2009-10-21 0.03215075 Au 34.19875 US$\n" ++
  "\n" ++
  "  'name1' (and 'name2', if specified) must be specified in the ledger\n" ++
  "  by 'ccs' records.\n" ++
  "\n" ++
  "* 'split date name amount1 amount2' records are used to specify stock\n" ++
  "  splits: 'name' is the name of the security being split, which must\n" ++
  "  be specified in the ledger by a 'ccs' record, 'date' is the date of\n" ++
  "  the split, and 'amount1' and 'amount2' are the new and old numbers\n" ++
  "  of shares, respectively. In the markets, these are usually integers,\n" ++
  "  but the program does not require this. For example, General Electric\n" ++
  "  did a 3:1 stock split on May 8, 2000, which could be specified as\n" ++
  "\n" ++
  "        split 2000-5-8 GE 3 1\n" ++
  "\n" ++
  "  but if you're feeling perverse you could also write any of\n" ++
  "\n" ++
  "        split 2000-5-8 GE 1 1/3\n" ++
  "        split 2000-5-8 GE 1.5 0.5\n" ++
  "        split 2000-5-8 GE 15/7 5/7\n" ++
  "\n" ++
  "* '[period] xfer [rec] date name1 name2 amount [name] [desc] [id]'\n" ++
  "  records are used to transfer 'amount' of 'name' from account 'name1'\n" ++
  "  to account 'name2'; 'name1' may be either an account specified by an\n" ++
  "  'account' record, or a source specified by an 'income' record. If\n" ++
  "  you don't specify the name of what's being transferred, the program\n" ++
  "  assumes it's the default ccs, US$ or Zorkmids or whatever. For\n" ++
  "  example, this record from my ledger file\n" ++
  "\n" ++
  "        xfer* 2009/2/27 interest abc:savings 0.01\n" ++
  "\n" ++
  "  says that on February 27, 2009, one penny interest was credited to\n" ++
  "  my ABC Bank savings account, and this record\n" ++
  "\n" ++
  "        xfer* 2009/9/26 checking utility:water 43.99 1216\n" ++
  "\n" ++
  "  is the payment of my water bill, in the amount of US$ 43.99, with\n" ++
  "  check #1216.\n" ++
  "\n" ++
  "  Both of these are marked as reconciled; this affects only the\n" ++
  "  'reconcile' command: when that command is run, reconciled\n" ++
  "  transactions are included in the reconciled balance and are not\n" ++
  "  printed, whereas not-reconciled transactions are printed but not\n" ++
  "  included in the reconciled balance.\n" ++
  "\n" ++
  "  The second form of the 'xfer' record allows specification of\n" ++
  "  multiple transfers as one logical transaction.\n" ++
  "\n" ++
  "  For details on the optional [period] prefix, see below.\n" ++
  "\n" ++
  "* '[period] exch [rec] date name amount1 name1 amount2 [name2] [desc]'\n" ++
  "  records and their aliases 'buy' and 'sell' are used to trade some\n" ++
  "  amount of one ccs for another. To some extent, these are syntactic\n" ++
  "  sugar: the same could be accomplished with a pair of 'xfer' records,\n" ++
  "  but this is a little clearer and less error-prone. In 'exch' and\n" ++
  "  'buy' records, (amount1,name1) is the new quantity coming into the\n" ++
  "  account named 'name', and (amount2,name2) is the quantity leaving\n" ++
  "  the account; for example\n" ++
  "\n" ++
  "        exch 2009-7-10 brokerage 300 MTLQQ.PK 300 GM \"rename GM\"\n" ++
  "\n" ++
  "  might be an entry describing the name change of GM shares when GM\n" ++
  "  went through bankruptcy (I'm not sure of the date though, and in any\n" ++
  "  case shareholders of \"old GM\" wouldn't simply get shares of \"new\n" ++
  "  GM\"... but ignore that), and\n" ++
  "\n" ++
  "        buy 2009/10/2 brokerage 3.959 VTSMX 100\n" ++
  "\n" ++
  "  might be an entry describing a regular automatic purchase of\n" ++
  "  VTSMX. For 'sell' records, the order is reversed: the first pair\n" ++
  "  (amount1,name1) describes what's being sold, ie, going out of the\n" ++
  "  account, and the second pair (amount2,name2) shows what's being\n" ++
  "  acquired in exchange. Thus\n" ++
  "\n" ++
  "        sell 2009/10/2 brokerage 3.959 VTSMX 100\n" ++
  "\n" ++
  "  would undo the previous 'buy' transaction. These could both be\n" ++
  "  written as 'exch' instead, as follows; I've added the explicit\n" ++
  "  specifier of US$.\n" ++
  "\n" ++
  "        exch 2009/10/2 brokerage 3.959 VTSMX 100 US$\n" ++
  "        exch 2009/10/2 brokerage 100 US$ 3.959 VTSMX\n" ++
  "\n" ++
  "  Again, for details on the optional [period] prefix, see below.\n" ++
  "\n" ++
  "* 'todo [rec] date text' is basically a sticky note in the ledger. If\n" ++
  "  the record is not marked as reconciled, and the date falls within\n" ++
  "  the range of the command being executed, the text is printed out. If\n" ++
  "  the record is marked as reconciled, the text is not printed out; the\n" ++
  "  record merely serves as a comment in the ledger. This is for leaving\n" ++
  "  yourself reminders of stuff that needs to be done at some time: for\n" ++
  "  example, my ledger file has entries\n" ++
  "\n" ++
  "        todo 2009/12/1 Start actively gathering tax info\n" ++
  "        todo 2010/4/10 Taxes better be done!!!\n" ++
  "\n" ++
  "  yet I won't be bothered by seeing these until those dates have\n" ++
  "  passed (or if I do a query for some time in the future).\n" ++
  "\n" ++
  "  In addition, there are two not-quite-financial bits of syntactic\n" ++
  "  sugar: there are two record types 'birthday' and 'anniversary', with\n" ++
  "  syntax otherwise just like 'todo', that act somewhat similarly. Both\n" ++
  "  of these are essentially periodic 'todo' records, however they only\n" ++
  "  trigger in a limited window of one week around each annual\n" ++
  "  recurrence of the given date. In addition, only the month and day\n" ++
  "  are printed, not the year. Thus, if there were a record\n" ++
  "\n" ++
  "        birthday 1928-11-18 Mickey Mouse\n" ++
  "\n" ++
  "  in the ledger file, a note would be printed for one week around each\n" ++
  "  November 18, formatted as\n" ++
  "\n" ++
  "        birthday 11/18 Mickey Mouse\n" ++
  "\n" ++
  "  There is no difference between 'birthday' and 'anniversary' except\n" ++
  "  the keyword used to enter them and the same keyword used to print\n" ++
  "  them. As in 'todo' records, the reconciliation mark serves to\n" ++
  "  suppress these.\n" ++
  "\n" ++
  "In the above:\n" ++
  "\n" ++
  "* 'name' is a sequence of non-blank characters whose first character\n" ++
  "  is a letter or a currency symbol and whose remaining characters are\n" ++
  "  letters or currency symbols or digits or symbols from the set\n" ++
  "  !%&*+-/:<=>?@^_~\n" ++
  "\n" ++
  "* 'date' is a date formatted as Y/M/D or Y-M-D, where no assumptions\n" ++
  "  are made about implicit century years etc: what you enter is what's\n" ++
  "  there, so 98-1-27, for example, is a date describing the death of\n" ++
  "  the roman emperor Nerva and the beginning of the reign of his\n" ++
  "  successor Trajan, not January 27, 1998. The program knows that there\n" ++
  "  are 12 months in a year, and how many days there are in each month\n" ++
  "  (including leap years, for which it assumes the use of the proleptic\n" ++
  "  Gregorian calendar), and it checks these for validity; it makes no\n" ++
  "  assumptions about the validity of the year. Years BC cannot be\n" ++
  "  entered... this is probably not a significant limitation.\n" ++
  "\n" ++
  "* 'date-range' is a pair of dates specified in one of several ways.\n" ++
  "\n" ++
  "  The first (and originally the only) way is to simply specify two\n" ++
  "  dates: \"2009-4-17 2009-9-27\" is most of spring and summer of 2009.\n" ++
  "  In this version, either or both dates can be left out, in which case\n" ++
  "  defaults get used: if only one date is given, it is assumed to be\n" ++
  "  the end of the range, and the beginning is implicitly taken to be\n" ++
  "  \"the beginning of time\", an internal date several thousand years ago\n" ++
  "  (October 23, 4004 BC to be exact). If both dates are left off, the\n" ++
  "  range is assumed to be from the beginning of time until today,\n" ++
  "  whatever today's date is.\n" ++
  "\n" ++
  "  The second way to specify a date range is to specify an incomplete\n" ++
  "  date: specifying just a year+month, \"2010-5\", or just a year,\n" ++
  "  \"2010\", generates a range that is that month or that year. There is\n" ++
  "  a special case in that if the year+month is the current year+month,\n" ++
  "  then the range extends from the beginning of the month to today's\n" ++
  "  date, and similarly if just the year is the current year, then the\n" ++
  "  range extends from the beginning of the year to today's date. For\n" ++
  "  other year+month or year ranges, the range is the full month or year\n" ++
  "  respectively.\n" ++
  "\n" ++
  "  The last way to specify a date range is to give the literal keyword\n" ++
  "  'last' followed by a number which is the number of days into the\n" ++
  "  past: \"last 30\" means from 30 days ago until today.\n" ++
  "\n" ++
  "* 'desc' is an arbitrary string enclosed in double quotes: \"a\n" ++
  "  string\". It may contain embedded escaped characters and escaped\n" ++
  "  quotes, using a back-slash for escaping: \"this is a\\\" string with \\n\n" ++
  "  embedded quote and newline\".\n" ++
  "\n" ++
  "* 'amount' is a rational number formatted in one of several ways: one\n" ++
  "  or more digits, possibly followed by a decimal point '.' and zero or\n" ++
  "  more digits ('\\d+(\\.\\d*)?' in perl regexp-speak), or a decimal point\n" ++
  "  '.' followed by one or more digits ('\\.\\d+'), or two sequences of\n" ++
  "  one or more digits each separated by '/' ('\\d+\\/\\d+'). In all cases,\n" ++
  "  an optional sign, '+' or '-', may precede the rational number.\n" ++
  "\n" ++
  "* 'rec' is a reconciliation mark: a '*' or a '!'. It may immediately\n" ++
  "  follow the record type, or it may be separated from the record type\n" ++
  "  by whitespace: ie, both 'todo*' and 'todo *' are legal.\n" ++
  "\n" ++
  "* 'id' (in an 'xfer' record) is a sequence of digits: a check number\n" ++
  "  or other identifying number.\n" ++
  "\n" ++
  "* 'period' is an optional prefix of 'xfer' and 'exch' records: it\n" ++
  "  specifies that this is a periodic transaction. It has the format\n" ++
  "\n" ++
  "        recurring interval until end-date [reconciled rec-date]\n" ++
  "\n" ++
  "  where 'recurring', 'until', and 'reconciled' are literal keywords,\n" ++
  "  end-date and rec-date are dates in the usual format described above,\n" ++
  "  and interval specifies how far apart the repetitions are: interval\n" ++
  "  may be any one of the literal keywords 'daily', 'weekly', 'monthly',\n" ++
  "  'quarterly', 'annually', 'biweekly', 'bimonthly', 'biannually',\n" ++
  "  'semiweekly', 'semimonthly', or 'semiannually', or an integer\n" ++
  "  followed by one of the literal keywords 'days', 'weeks', 'months',\n" ++
  "  or 'years'. For example\n" ++
  "\n" ++
  "        recurring monthly until 2010-12-31 \\\n" ++
  "            \\ xfer 2009-2-27 interest abc:savings 0.01\n" ++
  "\n" ++
  "  is a monthly interest payment into abc:savings, occurring on the\n" ++
  "  27th of each month from Feb 2009 until Dec 2010.\n" ++
  "\n" ++
  "  The 'reconciled' keyword and rec-date are themselves optional. If\n" ++
  "  present, they indicate that instances of this record before the\n" ++
  "  rec-date are reconciled.\n"
