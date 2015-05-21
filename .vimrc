" vim: foldmethod=marker:foldlevel=1

" Base settings {{{
set nocompatible
syntax enable
filetype plugin indent on

let mapleader = "\<Space>"

" Required dirs {{{
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

set regexpengine=1 " use new regex (vim >7.4), improves syntax speed
set number
set relativenumber
set splitbelow
set splitright
set mouse=a
set ttymouse=xterm2
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
set foldmethod=indent
set foldcolumn=2
set foldlevelstart=99
set foldnestmax=6
set scrolloff=3
set sidescrolloff=7
set display+=lastline
set encoding=utf-8
set autoread
set fileformats+=mac
set history=1000
set tabpagemax=50
set viminfo^=!
set sessionoptions-=help
set sessionoptions-=options
set sessionoptions-=tabpages
set directory=$HOME/.vim/swapfiles// " Directory to put temp file in
set viewoptions=folds,options,cursor,unix,slash " unix/windows compatibility
set hidden " Buffer becomes hidden when it is abandoned
set cursorline " Highlight the screen line of the cursor
set updatetime=500 " Miliseconds to wait before writing swap and triggering CursorHold
set laststatus=2 " always show status line
set background=dark
colorscheme distinguished

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
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

if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
endif
" }}}

" Plugins {{{
" VimPlug automatic installation {{{
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
" }}}

let g:plug_window = 'above new'

call plug#begin('~/.vim/bundle')

Plug 'Shougo/vimproc.vim', {'do': 'make'}

if !has('nvim')
  Plug 'Shougo/neocomplete.vim' " Next generation completion framework after neocomplcache
" neocomplete {{{
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#sources#syntax#min_keyword_length = 2
" }}}
endif

Plug 'kien/ctrlp.vim' " Fuzzy file, buffer, mru, tag, etc finder.
" CtrlP {{{
nnoremap <silent> <leader>u? :echo "[ ]all [.]last Buffert[A]g [B]uffer [D]ir [L]ine [M]enu [P]roject [T]ag MR[U] [Y]ank"<cr>
nnoremap <silent> <leader>u<space> :CtrlPMixed<cr>
nnoremap <silent> <leader>ua :CtrlPBufTagAll<cr>
nnoremap <silent> <leader>ub :CtrlPBuffer<cr>
nnoremap <silent> <leader>ud :CtrlPDir<cr>
nnoremap <silent> <leader>ul :CtrlPLine<cr>
nnoremap <silent> <leader>ut :CtrlPTag<cr>
nnoremap <silent> <leader>uu :CtrlPMRUFiles<cr>
nnoremap <silent> <leader>u. :CtrlPLastMode --dir<cr>
" }}}

Plug 'sgur/ctrlp-extensions.vim' " Plugins for ctrlp.vim
" CtrlP Extensions {{{
nnoremap <silent> <leader>um :CtrlPMenu<cr>
nnoremap <silent> <leader>uy :CtrlPYankring<cr>
" }}}

Plug 'okcompute/vim-ctrlp-session' " CtrlP extension to manage Vim sessions
" CtrlP Session {{{
nnoremap <silent> <leader>up :CtrlPSession<cr>
let g:ctrlp_session_path = '~/.vim/session'
" }}}

Plug 'honza/vim-snippets' " vim-snipmate default snippets (Previously snipmate-snippets)

Plug 'SirVer/ultisnips' " The ultimate snippet solution for Vim
" UltiSnips {{{
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
" }}}

Plug 'tpope/vim-commentary' " commentary.vim: comment stuff out

" netrw {{{
nnoremap <silent><f1> :Vexplore<cr>
nnoremap <silent><leader>f :Explore<cr>
let g:netrw_preview   = 1
let g:netrw_liststyle = 3 " tree
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

Plug 'majutsushi/tagbar' " Vim plugin that displays tags in a window, ordered by scope
" Tagbar {{{
nnoremap <silent> <f2> :TagbarToggle<cr>

let g:tagbar_type_coffee = {
      \ 'ctagstype' : 'coffee',
      \ 'kinds'     : [ 'c:classes', 'm:methods', 'f:functions', 'v:variables', 'f:fields' ]
      \}

let g:tagbar_type_puppet = {
      \ 'ctagstype': 'puppet',
      \ 'kinds': [ 'c:class', 's:site', 'n:node', 'd:definition' ]
      \}

let g:tagbar_type_xslt = {
      \ 'ctagstype' : 'xslt',
      \ 'kinds' : [ 'v:variables', 't:templates' ]
      \}
" }}}

Plug 'scrooloose/syntastic' " Syntax checking hacks for vim
" Syntastic {{{
let g:syntastic_check_on_open = 1
let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['java', 'scala']}
" }}}

Plug 'mattn/gist-vim', {'on':'Gist'} " vimscript for gist

Plug 'mattn/webapi-vim' " vim interface to Web API (required for gist)

Plug 'gregsexton/gitv', {'on':'Gitv'} " gitk for Vim

Plug 'airblade/vim-gitgutter' " A Vim plugin which shows a git diff in the gutter (sign column) and stages/reverts hunks.
" Gitgutter {{{
nmap <Leader>ga <Plug>GitGutterStageHunk
nmap <Leader>gr <Plug>GitGutterRevertHunk
nmap <Leader>gv <Plug>GitGutterPreviewHunk
let g:gitgutter_override_sign_column_highlight = 0
" }}}

Plug 'tpope/vim-fugitive' " A Git wrapper so awesome, it should be illegal
" Fugitive {{{
" Delete fugitive buffers when we leave them
" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
autocmd BufReadPost fugitive://* set bufhidden=delete

" add a mapping on .. to view parent tree
autocmd User fugitive
      \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
      \   nnoremap <buffer> .. :edit %:h<CR> |
      \ endif
" }}}

Plug 'tpope/vim-dispatch', {'on':['Make','Dispatch','Focus','Start']} " asynchronous build and test dispatcher

Plug 'tpope/vim-surround' " surround.vim: quoting/parenthesizing made simple

Plug 'Raimondi/delimitMate' " provides insert mode auto-completion for quotes, parens, brackets, etc.

Plug 'godlygeek/tabular' " Vim script for text filtering and alignment

Plug 'junegunn/vim-easy-align' " A Vim alignment plugin
" Easy Align {{{
vmap <bar> <Plug>(EasyAlign)
nmap <Leader><bar> <Plug>(EasyAlign)
" }}}

Plug 'vim-scripts/Gundo', {'on':'GundoToggle'} " Visualize your undo tree.
" Gundo {{{
nnoremap <C-u> :GundoToggle<CR>
" }}}

Plug 'tpope/vim-repeat' " repeat.vim: enable repeating supported plugin maps with .

Plug 'wellle/targets.vim' " Vim plugin that provides additional text objects

Plug 'ggVGc/vim-fuzzysearch' " Makes search in vim fuzzy
" FuzzySearch {{{
map <Leader>/ :FuzzySearch<cr>
" }}}

Plug 'Lokaltog/vim-easymotion' " Vim motions on speed!
" EasyMotion {{{
let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion
let g:EasyMotion_smartcase = 1
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
nmap <Leader>s <Plug>(easymotion-s)
" }}}

Plug 'bling/vim-airline' " lean & mean status/tabline for vim that's light as air
" Airline {{{
let g:airline#extensions#tabline#enabled = 0 " Automatically displays tab line.
let g:airline_powerline_fonts = 1 " Integrating with powerline fonts
let g:airline#extensions#tabline#formatter = 'unique_tail_improved' " smartly uniquify buffers names with similar filename,
let g:airline_inactive_collapse=1 " collapse inactive windows to filename only
let g:airline#extensions#tabline#fnamecollapse = 0
let g:airline#extensions#tabline#tab_min_count = 1
" }}}

Plug 'godlygeek/csapprox' " Make gvim-only colorschemes work transparently in terminal vim

Plug 'flazz/vim-colorschemes' " one colorscheme pack to rule them all!

Plug 'oblitum/rainbow', {'on':'RainbowToggle'} " Rainbow Parentheses Improved
" Rainbow {{{
nnoremap <silent> <F9> :RainbowToggle<cr>
" }}}

Plug 'nathanaelkane/vim-indent-guides', {'on':'IndentGuidesToggle'} " A Vim plugin for visually displaying indent levels in code
" Indent Guides {{{
nnoremap <silent> <F10> :IndentGuidesToggle<cr>
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

Plug 'Keithbsmiley/investigate.vim' " A Vim plugin for looking up documentation
" Investigate {{{
nnoremap <silent> <leader><f1> :call investigate#Investigate()<CR>
" }}}

"Plug 'tmux-plugins/vim-tmux-focus-events' " Make terminal vim and tmux work better together.

Plug 'ludovicchabant/vim-gutentags' " A Vim plugin that manages your tag files
" Gutentags {{{
let g:gutentags_exclude = ['node_modules']
" }}}

" File types {{{
Plug 'rodjek/vim-puppet' " Puppet niceties for your Vim setup
Plug 'vim-scripts/JSON.vim' " A syntax highlighting file for JSON
Plug 'hallison/vim-markdown' " Markdown syntax highlight for Vim editor with snippets support
Plug 'tpope/vim-rails' " rails.vim: Ruby on Rails power tools
Plug 'tpope/vim-bundler' " bundler.vim: Lightweight support for Ruby's Bundler
Plug 'tpope/vim-endwise' " wisely add 'end' in ruby, endfunction/endif/more in vim script, etc
Plug 'derekwyatt/vim-scala' " My work on integration of Scala into Vim - not a ton here, but useful for me.
Plug 'mustache/vim-mustache-handlebars' " mustache and handlebars mode for vim http://mustache.github.io
Plug 'yaymukund/vim-rabl' " Treat RABL files as ruby files, with a little extra sugar for RABL-specific DSL methods.

Plug 'pangloss/vim-javascript' " Vastly improved Javascript indentation and syntax support in Vim
" Javascript {{{
let javascript_enable_domhtmlcss = 1
" }}}

Plug 'kchmck/vim-coffee-script' " CoffeeScript support for vim
" CoffeeScript {{{
let coffee_compile_vert = 1
let coffee_watch_vert = 1
let coffee_run_vert = 1
autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent
" }}}
" }}}

call plug#end()
" }}}

" Mappings {{{
nnoremap <Leader>? :echo "[F1] Filer : [F2] Tagbar : [F9] Rainbow : [F10] Indent Guides"<cr>

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>

" swap backticks (` works for columns aswell, while ' goes to start of line)
nnoremap ' `
nnoremap ` '

" tag navigation
nnoremap <Leader>] g<C-]>
vnoremap <Leader>] g<C-]>
nnoremap <Leader>[ <C-t>
nnoremap <Leader>= <C-w>}

" buffer management
nnoremap <Leader>q :bdelete<cr>
nnoremap <C-j> :bnext<cr>
nnoremap <C-k> :bprevious<cr>
nnoremap <C-Right> :bnext<cr>
nnoremap <C-Left>  :bprevious<cr>
" Leader-Tab to switch current/last buffers
nnoremap <silent> <Leader><C-I> :b#<cr>
nnoremap <Leader>b :ls<cr>:b<space>

" reselect pasted text
nnoremap <leader>v V`]

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" save all buffers
nnoremap <leader>w :wa<cr>

" system clipboard
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" C-c closes all plugin windows plus quickfix and preview window
nnoremap <silent> <C-c> :pclose<cr>:cclose<cr>:TagbarClose<cr>

" make Y consistent with C and D
nnoremap Y y$

" Will allow you to use :w!! to write to a file using sudo if you forgot to sudo
" vim file (it will prompt for sudo password when writing)
" http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %

" U = redo
nnoremap U <C-r>

" Starting from vim 7.3 undo can be persisted across sessions
" http://www.reddit.com/r/vim/comments/kz84u/what_are_some_simple_yet_mindblowing_tweaks_to/c2onmqe
if has("persistent_undo")
  set undodir=~/.vim/undodir
  set undofile

  if !isdirectory(&undodir)
    call mkdir(&undodir, 'p')
  endif
endif
" }}}

" Events {{{
" checks if file has changed outside vim, on cursor stop
autocmd CursorHold * checktime

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

