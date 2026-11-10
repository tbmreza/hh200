" Set tabstop to 4 spaces
setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab

" Start the hh200 language server for this buffer (idempotent).
" The spawn command lives in lua/hh200/init.lua — change it there, not here.
if has('nvim-0.8')
  lua require('hh200').start()
endif
