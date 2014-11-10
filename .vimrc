" vim: foldmethod=marker:foldlevel=0

" Base defaults {{{
set nocompatible
syntax enable
filetype plugin indent on

let mapleader = "-"

" Portability {{{
let s:is_windows = has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_macvim = has('gui_macvim')
" ensure correct shell in gvim
if s:is_windows && !s:is_cygwin
  set shell=c:\windows\system32\cmd.exe
endif
" }}}
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

call plug#begin('~/.vim/bundle')
let g:plug_window = 'above new'

" VimProc - Interactive command execution in Vim. {{{
Plug 'Shougo/vimproc.vim', {'do': 'make'}
" }}}

" Unite {{{
Plug 'Shougo/unite.vim' " Unite and create user interfaces
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
nnoremap <silent> <f3> :<C-u>Unite -no-split -buffer-name=buffers buffer<CR>
nnoremap <silent> <f4> :<C-u>Unite -no-split -buffer-name=files buffer file_mru bookmark file_rec/async:!<CR>

" Map , to the prefix for Unite
nnoremap [unite] <Nop>
nmap , [unite]
vnoremap [unite] <Nop>
vmap , [unite]

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
" vimfiler
nnoremap <silent> [unite]t :VimFiler -toggle -buffer-name=vimfiler<cr>
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
nnoremap <silent> [unite]: :<C-u>UniteResume<CR>
" Command list
nmap [unite]? :echo "[ ]main [A]sources [B]uf/mru [C]md c[D] [F]ile [G]rep [H]elp boo[K]mark [L]ine [M]ru fi[N]d [O]utline [P]session [R]egister file[T]ree mru/b[U]f [V]git [W]ord [Y]ank [:]quick-cmd [+]resume"<cr>
" }}}

" Grepping {{{
if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
  let g:ackprg = "ag --nogroup --column --smart-case --follow"
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts =
  \ '-i -S -C4 --line-numbers --nocolor --nogroup --hidden --ignore ' .
  \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts =
  \ '-i --nogroup --column --smart-case --no-heading --no-color -k -H -C4'
  let g:unite_source_grep_recursive_opt=''
endif
" }}}
" }}}

" Code completion {{{
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' } " A code-completion engine for Vim
let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
let g:ycm_complete_in_comments = 1
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_add_preview_to_completeopt = 1
" }}}

" Snippets {{{
Plug 'honza/vim-snippets' " vim-snipmate default snippets (Previously snipmate-snippets)

" UltiSnips {{{
Plug 'SirVer/ultisnips' " The ultimate snippet solution for Vim
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
" }}}
" }}}

" Comments {{{
"Plug 'scrooloose/nerdcommenter' " A plugin that allows for easy commenting of code for many filetypes
Plug 'tpope/vim-commentary' " commentary.vim: comment stuff out
" }}}

" Navigation {{{
Plug 'mileszs/ack.vim' " Vim plugin for the Perl module / CLI script 'ack'
"Plug 'scrooloose/nerdtree' " A tree explorer plugin for navigating the filesystem
"Plug 'jistr/vim-nerdtree-tabs' " NERDTree and tabs together in Vim, painlessly

" Vimfiler {{{
Plug 'Shougo/vimfiler.vim' " Powerful file explorer implemented by Vim script               <F1>
nnoremap <silent> <f1> :VimFilerExplorer -buffer-name=vimfiler<cr>
let g:loaded_netrwPlugin = 1
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_tree_leaf_icon = ' '
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_file_icon = '-'
let g:vimfiler_marked_file_icon = '*'
let g:vimfiler_force_overwrite_statusline = 0
let g:vimfiler_expand_jump_to_first_child = 0
autocmd FileType vimfiler nmap <buffer> <2-LeftMouse> <Plug>(vimfiler_edit_file)
" }}}

" Tagbar {{{
Plug 'majutsushi/tagbar' " Vim plugin that displays tags in a window, ordered by scope      <F2>
nnoremap <silent> <f2> :TagbarToggle<cr>
" }}}
" }}}

" Syntax checker {{{
Plug 'scrooloose/syntastic' " Syntax checking hacks for vim
let g:syntastic_check_on_open = 1
" }}}

" File types {{{
Plug 'rodjek/vim-puppet' " Puppet niceties for your Vim setup
Plug 'vim-scripts/JSON.vim' " A syntax highlighting file for JSON
Plug 'hallison/vim-markdown' " Markdown syntax highlight for Vim editor with snippets support
Plug 'tpope/vim-rails' " rails.vim: Ruby on Rails power tools
Plug 'tpope/vim-bundler' " bundler.vim: Lightweight support for Ruby's Bundler
Plug 'tpope/vim-endwise' " wisely add 'end' in ruby, endfunction/endif/more in vim script, etc

" Javascript {{{
Plug 'pangloss/vim-javascript' " Vastly improved Javascript indentation and syntax support in Vim
let javascript_enable_domhtmlcss = 1
" }}}

" CoffeeScript {{{
Plug 'kchmck/vim-coffee-script' " CoffeeScript support for vim
let coffee_compile_vert = 1
let coffee_watch_vert = 1
let coffee_run_vert = 1
autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent
" }}}
" }}}

" Versioning {{{
Plug 'mattn/gist-vim' " vimscript for gist
Plug 'mattn/webapi-vim' " vim interface to Web API (required for gist)
Plug 'gregsexton/gitv' " gitk for Vim
Plug 'airblade/vim-gitgutter' " A Vim plugin which shows a git diff in the gutter (sign column) and stages/reverts hunks.

" Fugitive {{{
Plug 'tpope/vim-fugitive' " A Git wrapper so awesome, it should be illegal

" Delete fugitive buffers when we leave them
" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
autocmd BufReadPost fugitive://* set bufhidden=delete

" add a mapping on .. to view parent tree
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif
" }}}
" }}}

" Build {{{
Plug 'tpope/vim-dispatch' " asynchronous build and test dispatcher
" }}}

" Text manipulation {{{
Plug 'tpope/vim-surround' " surround.vim: quoting/parenthesizing made simple
Plug 'Raimondi/delimitMate' " provides insert mode auto-completion for quotes, parens, brackets, etc.
Plug 'godlygeek/tabular' " Vim script for text filtering and alignment

" Easy Align {{{
Plug 'junegunn/vim-easy-align' " A Vim alignment plugin
vmap a <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)
" }}}

" Undo {{{
Plug 'vim-scripts/Gundo' " Visualize your undo tree.
nnoremap <C-u> :GundoToggle<CR>
" U = redo (I never use the undo-line stuff anyway)
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

" Vim Pasta {{{
Plug 'sickill/vim-pasta' " Pasting in Vim with indentation adjusted to destination context
let g:pasta_disabled_filetypes = ['python', 'coffee', 'yaml', 'unite']
" }}}
" }}}

" Movement {{{
Plug 'tpope/vim-repeat' " repeat.vim: enable repeating supported plugin maps with .

" EasyMotion {{{
Plug 'Lokaltog/vim-easymotion' " Vim motions on speed!
let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion
let g:EasyMotion_smartcase = 1
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
nmap s <Plug>(easymotion-s)
" }}}

" GoldenView {{{
Plug 'zhaocai/GoldenView.Vim' " Always have a nice view for vim split windows
let g:goldenview__enable_default_mapping=0
let g:goldenview__enable_at_startup=0
nmap <silent> <F5> <Plug>GoldenViewSwitchToggle
nmap <silent> <S-F5>  <Plug>GoldenViewSplit
nmap <silent> <F6> <Plug>GoldenViewResize
nmap <silent> <s-F6> <Plug>ToggleGoldenViewAutoResize
" }}}
" }}}

" Interface {{{
" Airline {{{
Plug 'bling/vim-airline' " lean & mean status/tabline for vim that's light as air
set laststatus=2 " always show status line
let g:airline#extensions#tabline#enabled = 1 " Automatically displays tab line.
let g:airline_powerline_fonts = 1 " Integrating with powerline fonts
let g:airline#extensions#tabline#formatter = 'unique_tail_improved' " smartly uniquify buffers names with similar filename,
let g:airline_inactive_collapse=1 " collapse inactive windows to filename only
let g:airline#extensions#tabline#fnamecollapse = 0
let g:airline#extensions#tabline#tab_min_count = 1
" }}}

" Colors {{{
set background=dark
colorscheme distinguished

Plug 'godlygeek/csapprox' " Make gvim-only colorschemes work transparently in terminal vim
Plug 'flazz/vim-colorschemes' " one colorscheme pack to rule them all!

" Rainbow {{{
Plug 'oblitum/rainbow' " Rainbow Parentheses Improved
nnoremap <silent> <F9> :RainbowToggle<cr>
" }}}
" }}}

" Indent Guides {{{
Plug 'nathanaelkane/vim-indent-guides' " A Vim plugin for visually displaying indent levels in code
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1
let g:indent_guides_enable_on_vim_startup=1
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

" Start screen {{{
Plug 'mhinz/vim-startify' " A fancy start screen for Vim
let g:startify_bookmarks = [ '~/.vimrc' ]
let g:startify_change_to_vcs_root = 1
let g:startify_relative_path = 1
let g:startify_skiplist = [ 'COMMIT_EDITMSG' ]
let g:startify_custom_header = map(split(system('toilet -f ivrit "Vim 7.4"'), '\n'), '"   ". v:val') + ['','']
" http://www.shlomifish.org/humour/fortunes/
let g:startify_custom_footer = ['',''] + map(split(system('fortune joel-on-software osp_rules paul-graham sharp-perl sharp-programming'), '\n'), '"   ". v:val')
let g:startify_list_order = [
        \ ['   My sessions:'],
        \ 'sessions',
        \ ['   Last recently opened files:'],
        \ 'files',
        \ ['   My bookmarks:'],
        \ 'bookmarks',
        \ ]
" }}}
" }}}

" Help {{{
Plug 'chrisbra/vim_faq' " The Vim FAQ from http://vimdoc.sourceforge.net/

" Investigate {{{
Plug 'Keithbsmiley/investigate.vim' " A Vim plugin for looking up documentation
nnoremap <silent> <leader><f1> :call investigate#Investigate()<CR>
" }}}
" }}}

call plug#end()
" }}}

" General settings {{{
set regexpengine=1 " use new regex (vim >7.4), improves syntax speed
set scrolloff=3 " number of screen lines to show around the cursor
set sidescrolloff=7 " minimal number of columns to keep left and right of the cursor
set sidescroll=1 " minimal number of columns to scroll horizontally
set number " show the line number for each line
set splitbelow " a new window is put below the current one
set splitright " a new window is put right of the current one
set mouse=a " list of flags for using the mouse
set ttymouse=xterm2 " "xterm", "xterm2", "dec" or "netterm"; type of mouse
set tabstop=2 " number of spaces a <Tab> in the text stands for
set shiftwidth=2 " number of spaces used for each step of (auto)indent
set smarttab " a <Tab> in an indent inserts 'shiftwidth' spaces
set expandtab " expand <Tab> to spaces in Insert mode
set autoindent " automatically set the indent of a new line
set history=1000 " how many command lines are remembered
set list " show <Tab> as ^I and end-of-line as $
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅ " list of strings used for list mode
set wildmode=list:longest " specifies how command line completion works (bash-like)
set wildmenu " command-line completion shows a list of matches
set wildignore=*.o,*.obj,*~ " list of patterns to ignore files for file name completion
set formatoptions-=o " don't continue comments when o/O
set foldmethod=indent " The kind of folding used for the current window.
set foldcolumn=2 " Folding column width (0 = off)
set foldlevelstart=99 " Fold level start (0 = fold all, 99 = fold none)
set foldnestmax=6 " Fold nesting max level
set hlsearch " Highlight searches
set incsearch " Incremental search (move while searching)
set directory=$HOME/.vim/swapfiles// " Directory to put temp file in
set viewoptions=folds,options,cursor,unix,slash " unix/windows compatibility
set hidden " Buffer becomes hidden when it is abandoned
set cursorline " Highlight the screen line of the cursor
" }}}

" Unite settings {{{
" those must come after plug#end() :(
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_selecta'])
call unite#custom#profile('default', 'context', { 'marked_icon':'✓'})
let g:unite_source_history_yank_enable = 1
let g:unite_enable_start_insert = 1
let g:unite_source_session_path = "~/.vim/session"
let g:unite_source_session_enable_auto_save = 1
let g:unite_cursor_line_highlight = 'TabLineSel'
if !isdirectory('~/.vim/session')
  call mkdir('~/.vim/session', 'p')
endif

" Set up some custom ignores
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/', 'git5/.*/review/', 'google/obj/', 'tmp/', '.sass-cache', 'node_modules/', 'bower_components/', 'dist/', '.git5_specs/', '.pyc', 'log/',
      \ ], '\|'))
" }}}

" Events {{{
" checks if file has changed outside vim, on cursor stop
autocmd CursorHold * checktime

" cursorline only for curent buffer
autocmd WinLeave * setlocal nocursorline
autocmd WinEnter * setlocal cursorline

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

" spell check when writing commit logs
autocmd filetype svn,*commit* setlocal spell

" http://vim.wikia.com/wiki/Change_vimrc_with_auto_reload
autocmd! BufWritePost .vimrc,_vimrc,vimrc source $MYVIMRC
" }}}

" Custom Mappings {{{
nnoremap <Leader>? :echo "[s] EasyMotion : [-hjkl] EasyMotion move : [A-jk] drag line : [A-hl] indent : [U] redo : [C-u] Gundo : [ff] toggle fold : [:w!!] sudo w : [-Y] checkout line"<cr>
nnoremap <Leader>?? :echo "[F1] VimFiler : [F2] Tagbar : [F3] Unite Buffers : [F4] Unite : [F5] GView toggle : [S-F5] GView Split : [F6] GView resize : [S-F6] GView autoresize : [F9] Rainbow"<cr>

nnoremap ; :

" buffer management
nnoremap <Leader>q :bdelete<cr>
nnoremap <C-j> :bnext<cr>
nnoremap <C-k> :bprevious<cr>
" C-Tab to switch current/last buffers
nnoremap <silent> <C-I> :b#<cr>

" reselect pasted text
nnoremap <leader>v V`]

" C-c closes all plugin windows plus quickfix
nnoremap <silent> <C-c> :cclose<cr>:GundoHide<cr>:UniteClose<cr>:VimFilerClose vimfiler<cr>:TagbarClose<cr>

" make <C-l> clear the highlight as well as redraw
nnoremap <C-l> :nohls<cr><C-l>
inoremap <C-l> <C-o>:nohls<cr>

" tt toggles folding - http://vim.wikia.com/wiki/Folding
nnoremap <silent> tt za
vnoremap tt zf

" make Y consistent with C and D
nnoremap Y y$

" Will allow you to use :w!! to write to a file using sudo if you forgot to sudo
" vim file (it will prompt for sudo password when writing)
" http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" move a line of text using Alt+jk, indent with Alt+hl
nnoremap <A-j> :m+<CR>==
nnoremap <A-k> :m-2<CR>==
nnoremap <A-h> <<
nnoremap <A-l> >>
inoremap <A-j> <Esc>:m+<CR>==gi
inoremap <A-k> <Esc>:m-2<CR>==gi
inoremap <A-h> <Esc><<`]a
inoremap <A-l> <Esc>>>`]a
vnoremap <A-j> :m'>+<CR>gv=gv
vnoremap <A-k> :m-2<CR>gv=gv
vnoremap <A-h> <gv
vnoremap <A-l> >gv
" }}}

