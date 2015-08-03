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

" Grep {{{
if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts = '-i -S -C4 --line-numbers --nocolor --nogroup --hidden --ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts = '-i --nogroup --column --smart-case --no-heading --no-color -k -H -C4'
  let g:unite_source_grep_recursive_opt=''
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

if !has('nvim')
  Plug 'Shougo/neocomplete.vim' " Next generation completion framework after neocomplcache
" neocomplete {{{
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#sources#syntax#min_keyword_length = 2
" }}}
endif

Plug 'Shougo/unite.vim' " Unite and create user interfaces
" Unite {{{
Plug 'Shougo/unite-outline' " outline source for unite.vim
Plug 'Shougo/unite-help' " help source for unite.vim
Plug 'Shougo/unite-session' " unite.vim session source
Plug 'Shougo/neomru.vim' " MRU plugin includes unite.vim MRU sources
Plug 'thinca/vim-unite-history' " A source of unite.vim for history of command/search.

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
" yank history
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<CR>
" commands
nnoremap <silent> [unite]: :<C-u>Unite -buffer-name=history -default-action=edit history/command command<CR>
" resume
nnoremap <silent> [unite]. :<C-u>UniteResume<CR>
" Command list
nmap [unite]? :echo "[ ]main [A]sources [B]uf/mru [C]md c[D] [F]ile [G]rep [H]elp boo[K]mark [L]ine [M]ru fi[N]d [O]utline [P]session [R]egister mru/b[U]f [V]git [W]ord [Y]ank [:]quick-cmd [.]resume"<cr>
" }}}
" }}}

Plug 'ctrlpvim/ctrlp.vim' " Fuzzy file, buffer, mru, tag, etc finder.
" CtrlP {{{
let g:ctrlp_mruf_relative = 1
nnoremap <silent> <leader>i? :echo "[ ]all [.]last Buffert[A]g [B]uffer [D]ir [L]ine [M]enu [Q]uickfiX [T]ag MR[U] [Y]ank"<cr>
nnoremap <silent> <leader>i<space> :CtrlPMixed<cr>
nnoremap <silent> <leader>ia :CtrlPBufTagAll<cr>
nnoremap <silent> <leader>ib :CtrlPBuffer<cr>
nnoremap <silent> <leader>id :CtrlPDir<cr>
nnoremap <silent> <leader>il :CtrlPLine<cr>
nnoremap <silent> <leader>iq :CtrlPQuickfix<cr>
nnoremap <silent> <leader>it :CtrlPTag<cr>
nnoremap <silent> <leader>iu :CtrlPMRUFiles<cr>
nnoremap <silent> <leader>i. :CtrlPLastMode --dir<cr>
" }}}

Plug 'sgur/ctrlp-extensions.vim' " Plugins for ctrlp.vim
" CtrlP Extensions {{{
nnoremap <silent> <leader>im :CtrlPMenu<cr>
nnoremap <silent> <leader>iy :CtrlPYankring<cr>
" }}}

"Plug 'okcompute/vim-ctrlp-session' " CtrlP extension to manage Vim sessions
"" CtrlP Session {{{
"nnoremap <silent> <leader>up :CtrlPSession<cr>
"" }}}

Plug 'FelikZ/ctrlp-py-matcher' " Fast vim CtrlP matcher based on python
" CtrlP Py-Matcher {{{
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
" }}}

Plug 'honza/vim-snippets' " vim-snipmate default snippets (Previously snipmate-snippets)

Plug 'SirVer/ultisnips' " The ultimate snippet solution for Vim
" UltiSnips {{{
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
" }}}

Plug 'mattn/emmet-vim' " emmet for vim

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
let g:syntastic_javascript_checkers = ['jshint']
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
let g:gitgutter_max_signs = 5000
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

Plug 'AndrewRadev/splitjoin.vim' " A vim plugin that simplifies the transition between multiline and single-line code
" SplitJoin {{{
let g:splitjoin_align = 1
let g:splitjoin_ruby_hanging_args = 0
" }}}

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

Plug 'kana/vim-textobj-user' " Vim plugin: Create your own text objects

Plug 'nelstrom/vim-textobj-rubyblock' " A custom text object for selecting ruby blocks. - ar/ir

Plug 'wellle/targets.vim' " Vim plugin that provides additional text objects

Plug 'ggVGc/vim-fuzzysearch' " Makes search in vim fuzzy
" FuzzySearch {{{
map <Leader>/ :FuzzySearch<cr>
" }}}

Plug 'haya14busa/incsearch.vim' " Improved incremental searching for Vim
" incsearch {{{
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
" }}}

Plug 'justinmk/vim-sneak' " The missing motion for Vim
" vim sneak {{{
nmap <Leader>z <Plug>Sneak_s
nmap <Leader>Z <Plug>Sneak_S
xmap <leader>z <Plug>Sneak_s
xmap <leader>Z <Plug>Sneak_S
" }}}

Plug 'Lokaltog/vim-easymotion' " Vim motions on speed!
" EasyMotion {{{
let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion
let g:EasyMotion_smartcase = 1
map <Leader>s <Plug>(easymotion-prefix)
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

Plug 'mhinz/vim-janah' " A dark colorscheme for Vim.

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

Plug 'ryanoasis/vim-devicons' " adds font icons to programming languages, libraries, and web developer filetypes
Plug 'Keithbsmiley/investigate.vim' " A Vim plugin for looking up documentation
" Investigate {{{
nnoremap <silent> <leader>hh :call investigate#Investigate()<CR>
" }}}

Plug 'KabbAmine/zeavim.vim' " Zeal for Vim
" Zeal {{{
let g:zv_disable_mapping = 1
nmap <leader>hz <Plug>Zeavim
vmap <leader>hz <Plug>ZVVisSelection
nmap <leader>hZ <Plug>ZVKeyword
nmap <leader>hx <Plug>ZVKeyDocset
" }}}

Plug 'mhinz/vim-sayonara' " Sane buffer/window deletion.
" Sayonara {{{
nnoremap <leader>q :Sayonara!<cr>
nnoremap <leader>Q :Sayonara<cr>
let g:sayonara_confirm_quit = 1
" }}}

"Plug 'tmux-plugins/vim-tmux-focus-events' " Make terminal vim and tmux work better together.

"Plug 'ludovicchabant/vim-gutentags' " A Vim plugin that manages your tag files
" Gutentags {{{
"let g:gutentags_exclude = ['node_modules']
" }}}

" File types {{{
Plug 'vim-ruby/vim-ruby' " Vim/Ruby Configuration Files
Plug 'rodjek/vim-puppet' " Puppet niceties for your Vim setup
Plug 'vim-scripts/JSON.vim' " A syntax highlighting file for JSON
Plug 'hallison/vim-markdown' " Markdown syntax highlight for Vim editor with snippets support
Plug 'tpope/vim-rails' " rails.vim: Ruby on Rails power tools
Plug 'tpope/vim-bundler' " bundler.vim: Lightweight support for Ruby's Bundler
Plug 'tpope/vim-endwise' " wisely add 'end' in ruby, endfunction/endif/more in vim script, etc
Plug 'derekwyatt/vim-scala' " My work on integration of Scala into Vim - not a ton here, but useful for me.
Plug 'mustache/vim-mustache-handlebars' " mustache and handlebars mode for vim http://mustache.github.io
Plug 'yaymukund/vim-rabl' " Treat RABL files as ruby files, with a little extra sugar for RABL-specific DSL methods.
Plug 'moll/vim-node' " Tools and environment to make Vim superb for developing with Node.js. Like Rails.vim for Node.
Plug 'marijnh/tern_for_vim', {'do': 'npm install'} " Tern plugin for Vim
Plug 'jelera/vim-javascript-syntax' " Enhanced javascript syntax file for Vim
Plug 'othree/javascript-libraries-syntax.vim' " Syntax for JavaScript libraries
Plug 'vim-scripts/JavaScript-Indent' " Javascript indenter (HTML indent is included)

" Plug 'pangloss/vim-javascript' " Vastly improved Javascript indentation and syntax support in Vim
" Pangloss Javascript {{{
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

" {{{ Colors
set background=dark
autocmd ColorScheme * highlight Normal guibg=NONE ctermbg=NONE
colorscheme janah
" }}}

" Unite settings {{{
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_selecta'])
call unite#custom#profile('default', 'context', { 'marked_icon':'✓'})
let g:unite_source_history_yank_enable = 1
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

" U = redo
nnoremap U <C-r>

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

