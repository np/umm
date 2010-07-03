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
syn region String   start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=ummId,ummLabel
syn match ummLabel  display contained "#\(\w\|-\)\+"
syn match ummId     display contained "[*&]\d\{4\}-\d\{2\}-\d\{2\}:-\?\d\+\(\.\d\d\?\)\?\(:[A-Z]\+\)\?"
syn match Comment   /[#;].*$/
syn match ummAmount /\<-\?\d\+\(\.\d*\)\?\>/
syn match Keyword   /account\|anniversary\|annually\|biannually\|bimonthly\|birthday\|biweekly\|buy\|ccs\|daily\|days\|exch\|expense\|group\|income\|monthly\|months\|price\|quarterly\|reconciled\|recurring\|sell\|semiannually\|semimonthly\|semiweekly\|split\|todo\|until\|weekly\|weeks\|xfer\|years/
syn match ummStar   /\*/
syn match ummBang   /!/

hi link ummDate   Structure
hi link ummAmount Number
hi link ummStar   Keyword
hi link ummBang   Identifier
hi link ummLabel  Identifier
hi link ummId     Type

let b:current_syntax = "umm"

" vim: ts=8
