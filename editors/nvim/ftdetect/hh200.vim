" hh200 scripts use the .hhs extension (see src/Hh200/hhs-grammar.md).
" .hh200 is kept as a legacy alias.
au BufRead,BufNewFile *.hhs,*.hh200 set filetype=hh200
