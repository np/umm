" Filename:    umm.vim
" Purpose:     Vim syntax file
" Language:    UMM
" Maintainer:  Nicolas Pouillard <nicolas.pouillard@gmail.com>
" URL:         <none>

" For version 6.x: Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn match ummDate   /\d\{4\}-\d\{1,2\}-\d\{1,2\}/
syn match String    /"\([^"\\]\|\\.\)*"/
syn match Comment   /[#;].*$/
syn match ummAmount /\<\d\+\(\.\d*\)\?\>/
syn match Keyword   /account\|anniversary\|annually\|biannually\|bimonthly\|birthday\|biweekly\|buy\|ccs\|daily\|days\|exch\|expense\|group\|income\|monthly\|months\|price\|quarterly\|reconciled\|recurring\|sell\|semiannually\|semimonthly\|semiweekly\|split\|todo\|until\|weekly\|weeks\|xfer\|years/
syn match ummRec    /\*/

hi link ummDate   Structure
hi link ummAmount Number
hi link ummRec    Identifier

let b:current_syntax = "umm"

" vim: ts=8
