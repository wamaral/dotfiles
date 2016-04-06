" vim: foldmethod=marker:foldlevel=1

" Base settings {{{
set nocompatible
syntax enable
filetype plugin indent on

let mapleader = "\<Space>"

" Required dirs {{{
if !isdirectory($HOME.'/.vim/autoload')
  call mkdir($HOME.'/.vim/autoload', 'p')
endif
if !isdirectory($HOME.'/.vim/bundle')
  call mkdir($HOME.'/.vim/bundle', 'p')
endif
if !isdirectory($HOME.'/.vim_sessions')
  call mkdir($HOME.'/.vim_sessions', 'p')
endif
if !isdirectory($HOME.'/.vim/swapfiles')
  call mkdir($HOME.'/.vim/swapfiles', 'p')
endif
if !isdirectory($HOME.'/.vim/undodir')
  call mkdir($HOME.'/.vim/undodir', 'p')
endif
" }}}

" use old regex (vim >7.4), improves syntax speed
set regexpengine=1
set number
set relativenumber
set splitbelow
set splitright
set mouse=a
set smarttab
set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set backspace=indent,eol,start
set complete-=i
set nrformats-=octal
set ttimeout
set ttimeoutlen=100
set hlsearch
set incsearch
set list
set listchars=tab:▷•,trail:•,nbsp:•
set laststatus=2
set ruler
set showcmd
set wildmenu
set wildmode=list:longest
set wildignore=*.o,*.obj,*~
set formatoptions-=o
set formatoptions+=j
set foldmethod=indent
set foldcolumn=2
set foldlevelstart=99
set foldnestmax=6
set scrolloff=3
set sidescrolloff=7
set display+=lastline
set encoding=utf-8
set fileformats+=mac
set history=1000
set tabpagemax=50
set viminfo^=!
set sessionoptions-=help
set sessionoptions-=options
set sessionoptions-=tabpages
" Directory to put temp file in
set directory=$HOME/.vim/swapfiles//
" unix/windows compatibility
set viewoptions=folds,options,cursor,unix,slash
" Buffer becomes hidden when it is abandoned
set hidden
" Highlight the screen line of the cursor
set cursorline
" Miliseconds to wait before writing swap and triggering CursorHold
set updatetime=500
" always show status line
set laststatus=2
if !has('nvim')
set ttymouse=xterm2
endif

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
  set t_Co=16
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" Grep {{{
if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
endif
" }}}
" }}}

" Plugins {{{
" VimPlug automatic installation {{{
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
" }}}

let g:plug_window = 'above new'

call plug#begin('~/.vim/bundle')

Plug 'Shougo/vimproc.vim', {'do': 'make'}

if has('nvim')
  " Dark powered asynchronous completion framework for neovim
  Plug 'Shougo/deoplete.nvim'
  " deoplete {{{
  let g:deoplete#enable_at_startup = 1
  " }}}
else
  " Next generation completion framework after neocomplcache
  Plug 'Shougo/neocomplete.vim'
  " neocomplete {{{
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#sources#syntax#min_keyword_length = 3
  "}}}
endif

" A command-line fuzzy finder written in Go
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" fzf :heart: vim
Plug 'junegunn/fzf.vim'
" fzf.vim {{{
nnoremap <leader>ff :Files<cr>
nnoremap <leader>fp :GitFiles<cr>
nnoremap <leader>bb :Buffers<cr>
nnoremap <leader>s  :Ag<space><c-r><c-w>
xnoremap <leader>s  <esc>:Ag<space><c-r><c-w><cr>
nnoremap <leader>t  :Tags<space><c-r><c-w>
xnoremap <leader>t  <esc>:Tags<space><c-r><c-w><cr>

let g:fzf_layout = { 'down': '~40%' }
let g:fzf_buffers_jump = 1
" }}}

" commentary.vim: comment stuff out
Plug 'tpope/vim-commentary'

" netrw {{{
nnoremap <silent><leader>fv :Vexplore<cr>
nnoremap <silent><leader>fx :Explore<cr>
let g:netrw_preview   = 1
let g:netrw_winsize   = 25

autocmd FileType netrw call s:setup_netrw()
function! s:setup_netrw() abort
  nnoremap <buffer> <esc> :Rexplore<cr>
endfunction

autocmd User WinLeave * call s:quit_netrw()
function! s:quit_netrw()
  for i in range(1, bufnr($))
    if buflisted(i)
      if getbufvar(i, '&filetype') == "netrw"
        silent exe 'bwipeout ' . i
      endif
    endif
  endfor
endfunction
" }}}

" Syntax checking hacks for vim
Plug 'scrooloose/syntastic'
" Syntastic {{{
let g:syntastic_check_on_open = 1
let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['java', 'scala']}
let g:syntastic_javascript_checkers = ['eslint']
" }}}

" A Vim plugin which shows a git diff in the gutter (sign column) and stages/reverts hunks.
Plug 'airblade/vim-gitgutter'
" Gitgutter {{{
nmap <leader>ga <Plug>GitGutterStageHunk
nmap <leader>gr <Plug>GitGutterRevertHunk
nmap <leader>gv <Plug>GitGutterPreviewHunk
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_max_signs = 5000
" }}}

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'
" Fugitive {{{
nmap <leader>g? :echo "[b]lame [d]iff [s]tatus [p]ush :: st[a]ge-hunk pre[v]iew-hunk [r]evert-hunk :: [c = prev change :: ]c = next change"<cr>
nmap <leader>gd :Gdiff<cr>
nmap <leader>gb :Gblame<cr>
nmap <leader>gs :Gstatus<cr>
nmap <leader>gp :Gpush<cr>
" Delete fugitive buffers when we leave them
" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
autocmd BufReadPost fugitive://* set bufhidden=delete

" add a mapping on .. to view parent tree
autocmd User fugitive
      \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
      \   nnoremap <buffer> .. :edit %:h<cr> |
      \ endif
" }}}

" asynchronous build and test dispatcher
Plug 'tpope/vim-dispatch', {'on':['Make','Dispatch','Focus','Start']}

" surround.vim: quoting/parenthesizing made simple
Plug 'tpope/vim-surround'

" provides insert mode auto-completion for quotes, parens, brackets, etc.
Plug 'Raimondi/delimitMate'

" repeat.vim: enable repeating supported plugin maps with .
Plug 'tpope/vim-repeat'

" Vim plugin: Create your own text objects
Plug 'kana/vim-textobj-user'

" A custom text object for selecting ruby blocks. - ar/ir
Plug 'nelstrom/vim-textobj-rubyblock'

" Vim plugin that provides additional text objects
Plug 'wellle/targets.vim'

" Speed up Vim by updating folds only when just.
Plug 'Konfekt/FastFold'

" A light and configurable statusline/tabline for Vim
Plug 'itchyny/lightline.vim'
" lightline {{{
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ],
      \   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ },
      \ 'component_expand': {
      \   'syntastic': 'SyntasticStatuslineFlag',
      \ },
      \ 'component_type': {
      \   'syntastic': 'error',
      \ }
      \ }

let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0
" }}}

" A dark colorscheme for Vim.
Plug 'mhinz/vim-janah'

" A Vim plugin for visually displaying indent levels in code
Plug 'nathanaelkane/vim-indent-guides', {'on': 'IndentGuidesToggle'}
" Indent Guides {{{
nnoremap <silent> <f10> :IndentGuidesToggle<cr>
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1
let g:indent_guides_enable_on_vim_startup=0
let g:indent_guides_color_change_percent=3
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree', 'vimfiler', 'startify']
if !has('gui_running')
  let g:indent_guides_auto_colors=0
  function! s:indent_set_console_colors()
    hi IndentGuidesOdd ctermbg=235
    hi IndentGuidesEven ctermbg=236
  endfunction
  autocmd VimEnter,Colorscheme * call s:indent_set_console_colors()
endif
" }}}

" A Vim plugin for looking up documentation
Plug 'Keithbsmiley/investigate.vim'
" Investigate {{{
nnoremap <silent> <leader>hh :call investigate#Investigate()<CR>
" }}}

" Sane buffer/window deletion.
Plug 'mhinz/vim-sayonara'
" Sayonara {{{
nnoremap <leader>q :Sayonara!<cr>
nnoremap <leader>Q :Sayonara<cr>
let g:sayonara_confirm_quit = 1
" }}}

" EditorConfig plugin for Vim http://editorconfig.org
Plug 'editorconfig/editorconfig-vim'
" EditorConfig {{{
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
" }}}

" File types {{{
" Markdown syntax highlight for Vim editor with snippets support
Plug 'hallison/vim-markdown'

" ruby
" Vim/Ruby Configuration Files
Plug 'vim-ruby/vim-ruby'
" rails.vim: Ruby on Rails power tools
Plug 'tpope/vim-rails'
" bundler.vim: Lightweight support for Ruby's Bundler
Plug 'tpope/vim-bundler'
" wisely add 'end' in ruby, endfunction/endif/more in vim script, etc
Plug 'tpope/vim-endwise'
" Treat RABL files as ruby files, with a little extra sugar for RABL-specific DSL methods.
Plug 'yaymukund/vim-rabl'

"javascript
" Tools and environment to make Vim superb for developing with Node.js. Like Rails.vim for Node.
Plug 'moll/vim-node', {'for': 'javascript'}
" Tern plugin for Vim
Plug 'marijnh/tern_for_vim', {'do': 'npm install', 'for': 'javascript'}
" YAJS.vim: Yet Another JavaScript Syntax for Vim
Plug 'othree/yajs.vim', {'for': 'javascript'}
" Syntax for JavaScript libraries
Plug 'othree/javascript-libraries-syntax.vim', {'for': 'javascript'}
" JavaScript indentation for VIM
Plug 'gavocanov/vim-js-indent', {'for': 'javascript'}
" mustache and handlebars mode for vim http://mustache.github.io
Plug 'mustache/vim-mustache-handlebars'
" A syntax highlighting file for JSON
" Plug 'vim-scripts/JSON.vim'

" clojure
" fireplace.vim: Clojure REPL support
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
" salve.vim: static support for Leiningen and Boot
Plug 'tpope/vim-salve', {'for': 'clojure'}
" classpath.vim: Set 'path' from the Java class path
Plug 'tpope/vim-classpath', {'for': 'clojure'}
" Slamhound integration for vim.
Plug 'guns/vim-slamhound', {'for': 'clojure'}
" Additional IDE-like functionality for Clojure development using cider-nrepl
Plug 'Deraen/vim-cider', {'for': 'clojure'}
" A Vim plugin for Clojure's Eastwood linter
Plug 'venantius/vim-eastwood', {'for': 'clojure'}

" lisps
" Precision Editing for S-expressions
Plug 'guns/vim-sexp'
" vim-sexp mappings for regular people
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" Vastly improved Javascript indentation and syntax support in Vim
" Plug 'pangloss/vim-javascript'
" Pangloss Javascript {{{
"let javascript_enable_domhtmlcss = 1
" }}}

" CoffeeScript support for vim
Plug 'kchmck/vim-coffee-script'
" CoffeeScript {{{
let coffee_compile_vert = 1
let coffee_watch_vert = 1
let coffee_run_vert = 1
autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent
" }}}
" }}}

call plug#end()
" }}}

" {{{ Colors
set background=dark
autocmd ColorScheme * highlight Normal guibg=NONE ctermbg=NONE
colorscheme janah
" }}}

" Mappings {{{
nnoremap <leader>? :echo "[L-ff] Files : [L-fp] GitFiles : [L-bb] Buffers : [L-s] Ag : [L-t] Tags : [F9] Rainbow : [F10] Indent Guides"<cr>

nnoremap <silent> <C-l> :nohlsearch<cr><C-l>

" swap backticks (` works for columns aswell, while ' goes to start of line)
nnoremap ' `
nnoremap ` '

" tag navigation
nnoremap <leader>] g<C-]>
vnoremap <leader>] g<C-]>
nnoremap <leader>[ <C-t>
nnoremap <leader>= <C-w>}

" buffer management
nnoremap <C-j> :bnext<cr>
nnoremap <C-k> :bprevious<cr>
nnoremap <C-Right> :bnext<cr>
nnoremap <C-Left>  :bprevious<cr>
" Leader-Tab to switch current/last buffers
nnoremap <silent> <leader><C-i> :b#<cr>
nnoremap <leader>b :ls<cr>:b<space>

" reselect pasted text
nnoremap <leader>v V`]

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" fast save
nnoremap <leader>w :update<cr>
nnoremap <leader>fs :update<cr>
nnoremap <leader>W :wa<cr>

" system clipboard
vmap <leader>y "+y
vmap <leader>d "+d
nmap <leader>p "+p
nmap <leader>P "+P
vmap <leader>p "+p
vmap <leader>P "+P

" C-c closes all plugin windows plus quickfix and preview window
nnoremap <silent> <C-c> :pclose<cr>:cclose<cr>:TagbarClose<cr>

" make Y consistent with C and D
nnoremap Y y$

" Will allow you to use :w!! to write to a file using sudo if you forgot to sudo
" vim file (it will prompt for sudo password when writing)
" http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %

" Starting from vim 7.3 undo can be persisted across sessions
" http://www.reddit.com/r/vim/comments/kz84u/what_are_some_simple_yet_mindblowing_tweaks_to/c2onmqe
if has("persistent_undo")
  set undodir=~/.vim/undodir
  set undofile
endif
" }}}

" Events {{{
" checks if file has changed outside vim, on cursor stop
autocmd CursorHold * silent! checktime

" spell check when writing commit logs
autocmd filetype svn,*commit* setlocal spell

" jump to last cursor position when opening a file
" dont do it when writing a commit log entry
autocmd BufReadPost * call SetCursorPosition()
function! SetCursorPosition()
  if &filetype !~ 'svn\|commit\c'
    if line("'\"") > 0 && line("'\"") <= line("$")
      exe "normal! g`\""
      normal! zz
    endif
  end
endfunction
" }}}
