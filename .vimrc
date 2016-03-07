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
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '-i --vimgrep --hidden --ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts = '-i --no-heading --no-color -k -H'
  let g:unite_source_grep_recursive_opt = ''
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

" Unite and create user interfaces
Plug 'Shougo/unite.vim'
" Unite {{{
Plug 'Shougo/unite-outline' " outline source for unite.vim
Plug 'Shougo/unite-help' " help source for unite.vim
Plug 'Shougo/unite-session' " unite.vim session source
Plug 'Shougo/neomru.vim' " MRU plugin includes unite.vim MRU sources

" Unite buffers mappings {{{
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  let unite = unite#get_current_unite()
  nmap <buffer> <ESC> <Plug>(unite_exit)
  imap <buffer> <ESC> <Plug>(unite_insert_leave)
  imap <buffer> <C-j> <Plug>(unite_select_next_line)
  imap <buffer> <C-k> <Plug>(unite_select_previous_line)
  imap <buffer> <Down> <Plug>(unite_select_next_line)
  imap <buffer> <Up> <Plug>(unite_select_previous_line)
  nmap <buffer> p <Plug>(unite_toggle_auto_preview)
  imap <buffer> <C-p> <Plug>(unite_toggle_auto_preview)
  nmap <buffer> > <Plug>(unite_rotate_next_source)
  nnoremap <silent><buffer><expr> <C-i> unite#do_action('split')
  inoremap <silent><buffer><expr> <C-i> unite#do_action('split')
  nnoremap <silent><buffer><expr> <C-t> unite#do_action('tabopen')
  inoremap <silent><buffer><expr> <C-t> unite#do_action('tabopen')
  " sorter
  " from: https://github.com/LeafCage/dotfiles/blob/master/unite_setting.vim
  nnoremap <buffer><expr>sa unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_selecta'] : [])
  nnoremap <buffer><expr>sk unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_rank'] : [])
  nnoremap <buffer><expr>sr unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_reverse'] : [])
  nnoremap <buffer><expr>sw unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_word'] : [])
  nnoremap <buffer><expr>sl unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_length'] : [])
endfunction
" }}}

" Global mappings {{{
" Map the prefix for Unite
nnoremap [unite] <Nop>
nmap <Leader>u [unite]
vnoremap [unite] <Nop>
vmap <Leader>u [unite]

" General fuzzy search
nnoremap <silent> [unite]<space> :<C-u>Unite -buffer-name=files buffer file_rec/async:! file_mru bookmark<CR>
" sources
nnoremap <silent> [unite]a :<C-u>Unite -buffer-name=sources source<CR>
" buffers and mru
nnoremap <silent> [unite]b :<C-u>Unite -buffer-name=buffers buffer file_mru<CR>
" commands
nnoremap <silent> [unite]c :<C-u>Unite -buffer-name=commands command<CR>
" switch lcd
nnoremap <silent> [unite]d :<C-u>Unite -buffer-name=change-cwd -default-action=cd directory_mru directory_rec/async:! directory/new<CR>
" file search
nnoremap <silent> [unite]f :<C-u>Unite -buffer-name=files file_rec/async:! file/new<CR>
" grep from cwd
nnoremap <silent> [unite]g :<C-u>Unite -buffer-name=grep grep:.<CR>
" help
nnoremap <silent> [unite]h :<C-u>Unite -buffer-name=help help<CR>
" bookmarks
nnoremap <silent> [unite]k :<C-u>Unite -buffer-name=bookmarks bookmark<CR>
" line
nnoremap <silent> [unite]l :<C-u>Unite -buffer-name=search_file line<CR>
" MRU search
nnoremap <silent> [unite]m :<C-u>Unite -buffer-name=mru file_mru<CR>
" find
nnoremap <silent> [unite]n :<C-u>Unite -buffer-name=find find:.<CR>
" outline
nnoremap <silent> [unite]o :<C-u>Unite -buffer-name=outline -vertical outline<CR>
" sessions (projects)
nnoremap <silent> [unite]p :<C-u>Unite -buffer-name=sessions session session/new<CR>
" registers
nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=register register<CR>
" mru and buffers
nnoremap <silent> [unite]u :<C-u>Unite -buffer-name=buffers file_mru buffer<CR>
" git ls-files
nnoremap <silent> [unite]v :<C-u>Unite -buffer-name=git file_rec/git:--cached:--others:--exclude-standard<CR>
" grep word under cursor
nnoremap <silent> [unite]w :<C-u>Unite -buffer-name=grep grep:.::<C-R><C-w><CR>
vnoremap <silent> [unite]w "uy:let @u=substitute(@u, ':', '\\:', 'g')<CR>:<C-u>Unite -buffer-name=grep grep:.::<C-R>u<CR>
" commands
nnoremap <silent> [unite]: :<C-u>Unite -buffer-name=history -default-action=edit history/command command<CR>
" resume
nnoremap <silent> [unite]. :<C-u>UniteResume<CR>
" Command list
nmap [unite]? :echo "[ ]main [A]sources [B]uf/mru [C]md c[D] [F]ile [G]rep [H]elp boo[K]mark [L]ine [M]ru fi[N]d [O]utline [P]session [R]egister mru/b[U]f [V]git [W]ord [:]quick-cmd [.]resume"<cr>
" }}}
" }}}

" A command-line fuzzy finder written in Go
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

" commentary.vim: comment stuff out
Plug 'tpope/vim-commentary'

" netrw {{{
nnoremap <silent><f1> :Vexplore<cr>
nnoremap <silent><leader>f :Explore<cr>
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
nmap <Leader>ga <Plug>GitGutterStageHunk
nmap <Leader>gr <Plug>GitGutterRevertHunk
nmap <Leader>gv <Plug>GitGutterPreviewHunk
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_max_signs = 5000
" }}}

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'
" Fugitive {{{
nmap <Leader>g? :echo "[b]lame [d]iff [s]tatus [p]ush :: st[a]ge-hunk pre[v]iew-hunk [r]evert-hunk :: [c = prev change :: ]c = next change"<cr>
nmap <Leader>gd :Gdiff<cr>
nmap <Leader>gb :Gblame<cr>
nmap <Leader>gs :Gstatus<cr>
nmap <Leader>gp :Gpush<cr>
" Delete fugitive buffers when we leave them
" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
autocmd BufReadPost fugitive://* set bufhidden=delete

" add a mapping on .. to view parent tree
autocmd User fugitive
      \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
      \   nnoremap <buffer> .. :edit %:h<CR> |
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

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0
" }}}

" A dark colorscheme for Vim.
Plug 'mhinz/vim-janah'

" A Vim plugin for visually displaying indent levels in code
Plug 'nathanaelkane/vim-indent-guides', {'on': 'IndentGuidesToggle'}
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

" Unite settings {{{
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_selecta'])
call unite#custom#profile('default', 'context', { 'marked_icon':'✓'})
let g:unite_enable_start_insert = 1
let g:unite_source_session_path = $HOME . "/.vim_sessions"
let g:unite_source_session_enable_auto_save = 1
let g:unite_cursor_line_highlight = 'TabLineSel'

" Set up some custom ignores
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/', 'git5/.*/review/', 'google/obj/', 'tmp/', '.sass-cache', 'node_modules/', 'bower_components/', 'dist/', '.git5_specs/', '.pyc', 'log/',
      \ ], '\|'))
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

" fast save
nnoremap <leader>w :update<cr>
nnoremap <leader>fs :update<cr>
nnoremap <leader>W :wa<cr>

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

