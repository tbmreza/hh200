" Vim syntax file
" Language: hh200
" Maintainer: Jules
" Latest Revision: 2024

if exists("b:current_syntax")
  finish
endif

syn case ignore

" Comments
syn match hh200Comment "#.*$"

" Methods
syn keyword hh200Method GET POST PUT DELETE PATCH OPTIONS HEAD

" Keywords (Usually Case Sensitive? Keeping them sensitive unless specified otherwise.
" But 'syn case ignore' applies globally unless 'syn case match' is used.)
" Monarch file had methods as mixed case regex, but keywords as exact case.
" Let's keep keywords case sensitive if needed, but methods are case insensitive.

syn case match
syn keyword hh200Keyword then HTTP Configs Captures Asserts
syn case ignore

" Strings and URLs
syn match hh200Url "http[^ \t#]\+"
syn region hh200String start=+"+ skip=+\\"+ end=+"+

" Numbers
syn match hh200Number "\d\+"

" Identifiers (Variables?)
syn match hh200Identifier "[a-zA-Z_][a-zA-Z0-9\-_]*"

" Delimiters
syn match hh200Delimiter "[{}[\]():./=>]"

" JSONPath
syn match hh200JsonPath "\$[a-zA-Z0-9\._\[\]]*"

" Links
hi def link hh200Comment Comment
hi def link hh200Method Function
hi def link hh200Keyword Keyword
hi def link hh200String String
hi def link hh200Url Underlined
hi def link hh200Number Number
hi def link hh200Identifier Identifier
hi def link hh200Delimiter Delimiter
hi def link hh200JsonPath Special

let b:current_syntax = "hh200"
