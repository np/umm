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

$Id: UMMHelp.hs,v 1.37 2010/05/02 21:32:50 uwe Exp $ -}

module UMMHelp (writeHdr, usageMsg) where
import Prelude

version :: String
version = "0.2.0"

writeHdr :: String
writeHdr =
  "# Uwe's Money Manager -- umm " ++ version ++
  "\n# Copyright 2009-2010 Uwe Hollerbach <uh@alumni.caltech.edu>\n" ++
  "# Available under GPL V3 or later. Share and enjoy!\n" ++
  "# http://www.korgwal.com/umm/\n"

-- TODO: store these unadorned in some separate text files, and use
-- a program (Template Haskell?) to turn them into proper source code

usageMsg :: String -> String
usageMsg prog =
  "usage is '" ++ prog ++ " <ledger-file> <command> <options>'\n" ++
  "where <command> and <options> can be any of the following:\n" ++
  "\n" ++
  "    'balance' [account-or-group] [date]\n" ++
  "    'change' acc-or-inc-or-exp [date] [date]\n" ++
  "    'list' ['all' | 'accounts' | 'ccs' | 'expenses' | 'incomes' | 'groups']\n" ++
  "    'price' ccs [date]\n" ++
  "    'register' account [date] [date]\n" ++
  "    'reconcile' [account] [date]\n" ++
  "    'todo' [date]\n" ++
  "    'basis' ccs [date]\n" ++
  "    'export'\n" ++
  "\n" ++
  "The commands may be shortened to the unique prefixes or anything\n" ++
  "longer: 'b', 'ba', 'bala' etc, 'reg', 'regi' etc, but not 'r'.\n" ++
  "If no command or options are specified, the default action is\n" ++
  "to show the balances of all accounts as of the current date.\n" ++
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
  "--------\n" ++
  "\n" ++
  "* 'balance' shows the balance in the specified account or group,\n" ++
  "  or in all accounts if none is specified, as of the given date.\n" ++
  "\n" ++
  "\n" ++
  "* 'price' shows the price history of the specified currency, commodity,\n" ++
  "  or security, up to the specified date.\n" ++
  "\n" ++
  "\n" ++
  "* 'change' shows the change in the specified account or pseudo-account\n" ++
  "  between the two given dates, or from the beginning of the ledger to\n" ++
  "  the specified date.\n" ++
  "\n" ++
  "\n" ++
  "* 'price' shows the price history of the specified currency,\n" ++
  "  commodity, or security, up to the specified date.\n" ++
  "\n" ++
  "\n" ++
  "* 'register' shows all transactions involving the specified\n" ++
  "  account up to the given date, and shows the balance as of\n" ++
  "  that date.\n" ++
  "\n" ++
  "  If 'register' is given two dates, it prints transactions from\n" ++
  "  the earlier to the later date.\n" ++
  "\n" ++
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
  "\n" ++
  "* 'todo' shows all the unreconciled 'todo' items in the ledger\n" ++
  "  up to the date specified, defaulting to the current date\n" ++
  "\n" ++
  "\n" ++
  "* 'list' shows summaries of the various kinds of non-transaction\n" ++
  "  entries in the ledger file: currencies/commodities/securities,\n" ++
  "  income and expense categories, accounts, and groups\n" ++
  "\n" ++
  "\n" ++
  "* 'basis' shows the cost basis for a given currency or commodity\n" ++
  "  or security.\n" ++
  "\n" ++
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
  "    [period] 'xfer' [rec] date name1 {name2 amount [name],\\\n" ++
  "        \\ name3 amount [name], ...} [desc] [id]\n" ++
  "    [period] 'exch' [rec] date name amount1 name1 amount2 [name2] [desc]\n" ++
  "\n" ++
  "There are also 'buy' and 'sell' records which are just\n" ++
  "syntactic sugar for 'exch'; see more details below in the\n" ++
  "section describing the meaning of each type of record. The\n" ++
  "individual fields for each record are separated by whitespace.\n" ++
  "\n" ++
  "Blank lines or lines whose first non-blank character is '#'\n" ++
  "are treated as comments and are ignored.\n" ++
  "\n" ++
  "Syntactically, all of these are optional: there are no\n" ++
  "required records, and an empty ledger file is syntactically\n" ++
  "legal. However, a minimally-useful ledger file will probably\n" ++
  "contain at least some 'xfer' records, which in turn require\n" ++
  "that there be at least a couple of 'account' or 'income' or\n" ++
  "'expense' records.\n" ++
  "\n" ++
  "The order of records in the ledger file is not significant;\n" ++
  "the program orders them by type and date, and applies\n" ++
  "transactions in order by date.\n" ++
  "\n" ++
  "The second form of the 'xfer' record above shows splitting\n" ++
  "of records across multiple physical lines. All whitespace\n" ++
  "between the trailing '\\' on the first line and the leading '\\'\n" ++
  "on the second line, plus the two '\\', is annihilated and the\n" ++
  "remainders are joined.\n" ++
  "\n" ++
  "\n" ++
  "The meaning of each record type is as follows:\n" ++
  "\n" ++
  "* 'ccs name [desc] [amount] [name]' describes a currency or commodity\n" ++
  "  or security: the things you want to keep track of. The first\n" ++
  "  ccs record in the ledger file is the default unit; my ledger\n" ++
  "  file has 'ccs US$' very near the top. However, you don't\n" ++
  "  need one, in which case the program computes in Zorkmid\n" ++
  "  ('zm'). If you don't enter any 'ccs' record, you can't\n" ++
  "  specify any units in 'xfer' records, and you can't use any\n" ++
  "  'price' or 'split' records. You can use 'exch' (and 'buy'\n" ++
  "  and 'sell') records, but they probably won't be very useful,\n" ++
  "  since all you can do is to trade one amount of Zorkmids for\n" ++
  "  another. The 'desc' field in a ccs record is currently just\n" ++
  "  for documentation, it's not used by the program. The optional\n" ++
  "  'amount' field is for specifying the initial price of this ccs\n" ++
  "  in units of the second 'name' ccs, or the default ccs if no second\n" ++
  "  'name' is specified. If present, the second 'name' is also the ccs\n" ++
  "  into which this ccs is translated.\n" ++
  "\n" ++
  "\n" ++
  "* 'account name [date] [desc]' records specify accounts where you\n" ++
  "  want to keep of the quantity of what's in the account as well\n" ++
  "  as transactions which move stuff into or out of the account.\n" ++
  "  An account can contain multiple types of ccs, for example a\n" ++
  "  single account could be used to describe a brokerage account\n" ++
  "  containing many securities as well as cash. The date and desc\n" ++
  "  fields are currently just for documentation and are not used\n" ++
  "  by the program.\n" ++
  "\n" ++
  "\n" ++
  "* 'group name [name...]' groups multiple accounts together into one\n" ++
  "  group, so that it's possible to query the balances for a group of\n" ++
  "  accounts as a whole.\n" ++
  "\n" ++
  "\n" ++
  "* 'income name [desc]' and 'expense name [desc]' records specify\n" ++
  "  pseudo-accounts where you don't want to keep track of what's\n" ++
  "  inside, just the transactions: think of these as categories.\n" ++
  "  'income' specifies a source of ccs, and 'expense' specifies a\n" ++
  "  destination or sink. The desc field is currently just for\n" ++
  "  documentation. You can, to some extent, circumvent the source\n" ++
  "  or sink nature by entering a transaction from a source to an\n" ++
  "  account, but with a negative amount: this would actually take\n" ++
  "  money out of the account. This is intended for VERY OCCASIONAL\n" ++
  "  overrides, say you overpaid a utility bill and get a credit\n" ++
  "  which you don't want to just apply to the next bill. Don't\n" ++
  "  abuse this!\n" ++
  "\n" ++
  "\n" ++
  "* 'price date [amount1] name1 amount2 [name2]' records are\n" ++
  "  used to specify the price of one ccs in terms of another;\n" ++
  "  usually a currency. If the first amount is not specified,\n" ++
  "  it defaults to 1, and if the second name is not specified,\n" ++
  "  it defaults to the default ccs. For example, if you are\n" ++
  "  tracking ounces of gold using the ccs name 'Au', you might\n" ++
  "  have a price record\n" ++
  "\n" ++
  "        price 2009-10-21 Au 1063.70\n" ++
  "\n" ++
  "  and if you track ounces but for some reason have a price quote\n" ++
  "  in grams, you might write\n" ++
  "\n" ++
  "        price 2009-10-21 0.03215075 Au 34.19875 US$\n" ++
  "\n" ++
  "  'name1' (and 'name2', if specified) must be specified in the\n" ++
  "  ledger by 'ccs' records.\n" ++
  "\n" ++
  "\n" ++
  "* 'split date name amount1 amount2' records are used to specify\n" ++
  "  stock splits: 'name' is the name of the security being split,\n" ++
  "  which must be specified in the ledger by a 'ccs' record, 'date'\n" ++
  "  is the date of the split, and 'amount1' and 'amount2' are the\n" ++
  "  new and old numbers of shares, respectively. In the markets,\n" ++
  "  these are usually integers, but the program does not require\n" ++
  "  this. For example, General Electric did a 3:1 stock split on\n" ++
  "  May 8, 2000, which could be specified as\n" ++
  "\n" ++
  "        split 2000-5-8 GE 3 1\n" ++
  "\n" ++
  "  but if you're feeling perverse you could also write any of\n" ++
  "\n" ++
  "        split 2000-5-8 GE 1 1/3\n" ++
  "        split 2000-5-8 GE 1.5 0.5\n" ++
  "        split 2000-5-8 GE 15/7 5/7\n" ++
  "\n" ++
  "\n" ++
  "* '[period] xfer [rec] date name1 name2 amount [name] [desc] [id]'\n" ++
  "  records are used to transfer 'amount' of 'name' from account\n" ++
  "  'name1' to account 'name2'; 'name1' may be either an account\n" ++
  "  specified by an 'account' record, or a source specified by\n" ++
  "  an 'income' record. If you don't specify the name of what's\n" ++
  "  being transferred, the program assumes it's the default ccs,\n" ++
  "  US$ or Zorkmids or whatever. For example, this record from my\n" ++
  "  ledger file\n" ++
  "\n" ++
  "        xfer* 2009/2/27 interest abc:savings 0.01\n" ++
  "\n" ++
  "  says that on February 27, 2009, one penny interest was credited\n" ++
  "  to my ABC Bank savings account, and this record\n" ++
  "\n" ++
  "        xfer* 2009/9/26 checking utility:water 43.99 1216\n" ++
  "\n" ++
  "  is the payment of my water bill, in the amount of US$ 43.99,\n" ++
  "  with check #1216.\n" ++
  "\n" ++
  "  Both of these are marked as reconciled; this affects only\n" ++
  "  the 'reconcile' command: when that command is run, reconciled\n" ++
  "  transactions are included in the reconciled balance and are\n" ++
  "  not printed, whereas not-reconciled transactions are printed\n" ++
  "  but not included in the reconciled balance.\n" ++
  "\n" ++
  "  The second form of the 'xfer' record allows specification of\n" ++
  "  multiple transfers as one logical transaction.\n" ++
  "\n" ++
  "  For details on the optional [period] prefix, see below.\n" ++
  "\n" ++
  "\n" ++
  "* '[period] exch [rec] date name amount1 name1 amount2 [name2] [desc]'\n" ++
  "  records and their aliases 'buy' and 'sell' are used to trade\n" ++
  "  some amount of one ccs for another. To some extent, these\n" ++
  "  are syntactic sugar: the same could be accomplished with a\n" ++
  "  pair of 'xfer' records, but this is a little clearer and\n" ++
  "  less error-prone. In 'exch' and 'buy' records,\n" ++
  "  (amount1,name1) is the new quantity coming into the account\n" ++
  "  named 'name', and (amount2,name2) is the quantity leaving\n" ++
  "  the account; for example\n" ++
  "\n" ++
  "        exch 2009-7-10 brokerage 300 MTLQQ.PK 300 GM \"rename GM\"\n" ++
  "\n" ++
  "  might be an entry describing the name change of GM shares\n" ++
  "  when GM went through bankruptcy (I'm not sure of the date though,\n" ++
  "  and in any case shareholders of \"old GM\" wouldn't simply\n" ++
  "  get shares of \"new GM\"... but ignore that), and\n" ++
  "\n" ++
  "        buy 2009/10/2 brokerage 3.959 VTSMX 100\n" ++
  "\n" ++
  "  might be an entry describing a regular automatic purchase of\n" ++
  "  VTSMX. For 'sell' records, the order is reversed: the first\n" ++
  "  pair (amount1,name1) describes what's being sold, ie, going\n" ++
  "  out of the account, and the second pair (amount2,name2) shows\n" ++
  "  what's being acquired in exchange. Thus\n" ++
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
  "\n" ++
  "* 'todo [rec] date text' is basically a sticky note in the\n" ++
  "  ledger. If the record is not marked as reconciled, and the\n" ++
  "  date falls within the range of the command being executed,\n" ++
  "  the text is printed out. If the record is marked as\n" ++
  "  reconciled, the text is not printed out; the record merely\n" ++
  "  serves as a comment in the ledger. This is for leaving\n" ++
  "  yourself reminders of stuff that needs to be done at some\n" ++
  "  time: for example, my ledger file has entries\n" ++
  "\n" ++
  "        todo 2009/12/1 Start actively gathering tax info\n" ++
  "        todo 2010/4/10 Taxes better be done!!!\n" ++
  "\n" ++
  "  yet I won't be bothered by seeing these until those dates\n" ++
  "  have passed (or if I do a query for some time in the future).\n" ++
  "\n" ++
  "\n" ++
  "In the above:\n" ++
  "\n" ++
  "* 'name' is a sequence of non-blank characters whose first\n" ++
  "  character is a letter or a currency symbol and whose remaining\n" ++
  "  characters are letters or currency symbols or digits or symbols\n" ++
  "  from the set !%&*+-/:<=>?@^_~\n" ++
  "\n" ++
  "\n" ++
  "* 'date' is a date formatted as Y/M/D or Y-M-D, where no\n" ++
  "  assumptions are made about implicit century years etc:\n" ++
  "  what you enter is what's there, so 98-1-27, for example,\n" ++
  "  is a date describing the death of the roman emperor Nerva\n" ++
  "  and the beginning of the reign of his successor Trajan,\n" ++
  "  not January 27, 1998. The program knows that there are\n" ++
  "  12 months in a year, and how many days there are in each\n" ++
  "  month (including leap years, for which it assumes the use\n" ++
  "  of the proleptic Gregorian calendar), and it checks these\n" ++
  "  for validity; it makes no assumptions about the validity\n" ++
  "  of the year. Years BC cannot be entered... this is probably\n" ++
  "  not a significant limitation.\n" ++
  "\n" ++
  "\n" ++
  "* 'desc' is an arbitrary string enclosed in double quotes:\n" ++
  "  \"a string\". It may contain embedded escaped characters\n" ++
  "  and escaped quotes, using a back-slash for escaping:\n" ++
  "  \"this is a\\\" string with \\n embedded quote and newline\".\n" ++
  "\n" ++
  "\n" ++
  "* 'amount' is a rational number formatted in one of several\n" ++
  "  ways: one or more digits, possibly followed by a decimal\n" ++
  "  point '.' and zero or more digits ('\\d+(\\.\\d*)?' in perl\n" ++
  "  regexp-speak), or a decimal point '.' followed by one or\n" ++
  "  more digits ('\\.\\d+'), or two sequences of one or more\n" ++
  "  digits each separated by '/' ('\\d+\\/\\d+'). In all cases,\n" ++
  "  an optional sign, '+' or '-', may precede the rational\n" ++
  "  number.\n" ++
  "\n" ++
  "\n" ++
  "* 'rec' is a reconciliation mark: an asterisk '*'. It may\n" ++
  "  immediately follow the record type, or it may be separated\n" ++
  "  from the record type by whitespace: ie, both 'todo*' and\n" ++
  "  'todo *' are legal.\n" ++
  "\n" ++
  "\n" ++
  "* 'id' (in an 'xfer' record) is a sequence of digits: a\n" ++
  "  check number or other identifying number.\n" ++
  "\n" ++
  "\n" ++
  "* 'period' is an optional prefix of 'xfer' and 'exch' records:\n" ++
  "  it specifies that this is a periodic transaction. It has the\n" ++
  "  format\n" ++
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
  "  is a monthly interest payment into abc:savings, occurring on\n" ++
  "  the 27th of each month from February 2009 until December 2010.\n" ++
  "\n" ++
  "  The reconciled keyword and rec-date are themselves optional.\n" ++
  "  If present, they indicate that instances of this record before\n" ++
  "  the rec-date are reconciled.\n"
