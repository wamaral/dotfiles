if has('gui_running')
  " general settings
  set guioptions-=L " fix gvim resizing bug when opening tabs

  " gvim font
  set guifont=Inconsolata\ Medium\ 10

  " tabs
  nnoremap <f10> :tabnew<cr>
  nnoremap <f11> :tabp<cr>
  nnoremap <f12> :tabn<cr>
endif
