set nocompatible               " Be iMproved

"NeoBundle Scripts-----------------------------
if has('vim_starting')
  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
NeoBundle 'Shougo/vimproc.vim', {'build': {'unix': 'make'}} " Interactive command execution in Vim.

" Fuzzy search
NeoBundle 'Shougo/unite.vim' " Unite and create user interfaces
NeoBundle 'Shougo/unite-outline' " outline source for unite.vim
NeoBundle 'Shougo/unite-help' " help source for unite.vim
NeoBundle 'Shougo/unite-session' " unite.vim session source
NeoBundle 'Shougo/neomru.vim' " MRU plugin includes unite.vim MRU sources
NeoBundle 'thinca/vim-unite-history' " A source of unite.vim for history of command/search.
NeoBundle 'mileszs/ack.vim' " Vim plugin for the Perl module / CLI script 'ack'

" Code completion
NeoBundle 'Valloric/YouCompleteMe'
"NeoBundle 'Valloric/YouCompleteMe', {
"      \ 'build': {'unix': 'sh install.sh --clang-completer --system-libclang'},
"      \ } " A code-completion engine for Vim
"NeoBundle 'ervandew/supertab' " Perform all your vim insert mode completions with Tab

" Snippets
NeoBundle 'SirVer/ultisnips' " The ultimate snippet solution for Vim
NeoBundle 'honza/vim-snippets' " vim-snipmate default snippets (Previously snipmate-snippets)

" Comments
NeoBundle 'scrooloose/nerdcommenter' " A plugin that allows for easy commenting of code for many filetypes

" File browsing
NeoBundle 'scrooloose/nerdtree' " A tree explorer plugin for navigating the filesystem           <F1>
NeoBundle 'jistr/vim-nerdtree-tabs' " NERDTree and tabs together in Vim, painlessly

" Syntax checker
NeoBundle 'scrooloose/syntastic' " Syntax checking hacks for vim

" File types
NeoBundle 'kchmck/vim-coffee-script' " CoffeeScript support for vim
NeoBundle 'rodjek/vim-puppet' " Puppet niceties for your Vim setup
NeoBundle 'vim-scripts/JSON.vim' " A syntax highlighting file for JSON
NeoBundle 'hallison/vim-markdown' " Markdown syntax highlight for Vim editor with snippets support

" Versioning
NeoBundle 'tpope/vim-fugitive' " A Git wrapper so awesome, it should be illegal
NeoBundle 'mattn/gist-vim' " vimscript for gist
NeoBundle 'gregsexton/gitv' " gitk for Vim
NeoBundle 'airblade/vim-gitgutter' " A Vim plugin which shows a git diff in the gutter (sign column) and stages/reverts hunks.

" Text manipulation
NeoBundle 'tpope/vim-surround' " surround.vim: quoting/parenthesizing made simple
NeoBundle 'Raimondi/delimitMate' " provides insert mode auto-completion for quotes, parens, brackets, etc.
NeoBundle 'sickill/vim-pasta' " Pasting in Vim with indentation adjusted to destination context
NeoBundle 'godlygeek/tabular' " Vim script for text filtering and alignment

" Tags
NeoBundle 'majutsushi/tagbar' " Vim plugin that displays tags in a window, ordered by scope      <F3>

" Status line
NeoBundle 'bling/vim-airline' " lean & mean status/tabline for vim that's light as air

" Color themes
NeoBundle 'godlygeek/csapprox' " Make gvim-only colorschemes work transparently in terminal vim
NeoBundle 'altercation/vim-colors-solarized' " precision colorscheme for the vim text editor

" Undo
NeoBundle 'vim-scripts/Gundo' " Visualize your undo tree.


" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------


" syntax
syntax enable

" colors
set background=dark
colorscheme distinguished

" general settings
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
set smartindent " do clever autoindenting
set cindent " enable specific indenting for C code
set history=1000 " how many command lines are remembered
set list " show <Tab> as ^I and end-of-line as $
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅ " list of strings used for list mode
set wildmode=list:longest " specifies how command line completion works (bash-like)
set wildmenu " command-line completion shows a list of matches
set wildignore=*.o,*.obj,*~ " list of patterns to ignore files for file name completion
set formatoptions-=o " don't continue comments when o/O
set showtabline=2 " always show tab bar
set foldmethod=syntax " The kind of folding used for the current window.
set foldcolumn=2 " Folding column width (0 = off)
set foldlevelstart=99 " Fold level start (0 = fold all, 99 = fold none)
set foldnestmax=6 " Fold nesting max level
set hlsearch " Highlight searches
set incsearch " Incremental search (move while searching)
set directory=$HOME/.vim/swapfiles// " Directory to put temp file in
set cursorline " Highlight the screen line of the cursor

" menu in console
source $VIMRUNTIME/menu.vim
set cpoptions-=<
set wildcharm=<C-Z>
map <F5> :emenu <C-Z>

" airline settings
set laststatus=2 " always show status line
let g:airline#extensions#tabline#enabled = 1 " Automatically displays tab line.
let g:airline#extensions#tabline#show_buffers = 0 " Don't show buffers (we use tabs)
let g:airline_powerline_fonts = 1 " Integrating with powerline fonts
let g:airline#extensions#tabline#formatter = 'unique_tail_improved' " smartly uniquify buffers names with similar filename,

" syntastic settings
let g:syntastic_check_on_open = 1

" nerdtree settings
let g:NERDTreeMouseMode = 2
let g:NERDTreeWinSize = 40
let g:NERDTreeShowHidden = 0
let g:nerdtree_tabs_open_on_console_startup = 0
let g:nerdtree_tabs_open_on_gui_startup = 0
let g:nerdtree_tabs_no_startup_for_diff = 1
let g:nerdtree_tabs_smart_startup_focus = 1
let g:nerdtree_tabs_open_on_new_tab = 1 " if NERDTree was globally opened by :NERDTreeTabsToggle

" supertab settings
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabContextDefaultCompletionType = "<c-n>"
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" coffeescript settings
let coffee_compile_vert = 1
let coffee_watch_vert = 1
let coffee_run_vert = 1
autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent

" vim-pasta settings
let g:pasta_disabled_filetypes = ['python', 'coffee', 'yaml', 'unite']

" unite settings
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_source_history_yank_enable = 1
let g:unite_enable_start_insert = 1
let g:unite_source_session_enable_auto_save = 1
let g:unite_cursor_line_highlight = 'TabLineSel'
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  let b:SuperTabDisabled=1
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
  nnoremap <buffer><expr>sr unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_reverse'] : [])
  nnoremap <buffer><expr>sw unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_word'] : [])
  nnoremap <buffer><expr>sl unite#mappings#set_current_filters(empty(unite#mappings#get_current_filters()) ? ['sorter_length'] : [])
endfunction

" Unite bindings
" from: https://github.com/terryma/dotfiles/blob/master/.vimrc
" Set up some custom ignores
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/', 'git5/.*/review/', 'google/obj/', 'tmp/', '.sass-cache', 'node_modules/', 'bower_components/', 'dist/', '.git5_specs/', '.pyc',
      \ ], '\|'))
" Map , to the prefix for Unite
nnoremap [unite] <Nop>
nmap , [unite]
" General fuzzy search
nnoremap <silent> [unite]<space> :<C-u>Unite -buffer-name=files buffer file_mru bookmark file_rec/async<CR>
" Quick registers
nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=register register<CR>
" Quick buffer and mru
nnoremap <silent> [unite]u :<C-u>Unite -buffer-name=buffers file_mru buffer<CR>
" Quick yank history
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<CR>
" Quick outline
nnoremap <silent> [unite]o :<C-u>Unite -buffer-name=outline -vertical outline<CR>
" Quick sessions (projects)
nnoremap <silent> [unite]p :<C-u>Unite -buffer-name=sessions session session/new<CR>
" Quick sources
nnoremap <silent> [unite]a :<C-u>Unite -buffer-name=sources source<CR>
" Quickly switch lcd
nnoremap <silent> [unite]d :<C-u>Unite -buffer-name=change-cwd -default-action=cd directory_mru directory_rec/async<CR>
" Quick file search
nnoremap <silent> [unite]f :<C-u>Unite -buffer-name=files file_rec/async file/new<CR>
" Quick grep from cwd
nnoremap <silent> [unite]g :<C-u>Unite -buffer-name=grep grep:.<CR>
" Quick help
nnoremap <silent> [unite]h :<C-u>Unite -buffer-name=help help<CR>
" Quick line
nnoremap <silent> [unite]l :<C-u>Unite -buffer-name=search_file line<CR>
" Quick MRU search
nnoremap <silent> [unite]m :<C-u>Unite -buffer-name=mru file_mru<CR>
" Quick find
nnoremap <silent> [unite]n :<C-u>Unite -buffer-name=find find:.<CR>
" Quick commands
nnoremap <silent> [unite]c :<C-u>Unite -buffer-name=commands command<CR>
" Quick bookmarks
nnoremap <silent> [unite]b :<C-u>Unite -buffer-name=bookmarks bookmark<CR>
" Quick commands
nnoremap <silent> [unite]: :<C-u>Unite -buffer-name=history -default-action=edit history/command command<CR>



" map ; to : (get faster)
nnoremap ; :

" navigate splits with C-move
nnoremap <C-Left> <C-w>h
nnoremap <C-Down> <C-w>j
nnoremap <C-Up> <C-w>k
nnoremap <C-Right> <C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" move a line of text using Alt+[up|down], indent with Alt+[left|right]
nnoremap <A-Down> :m+<CR>==
nnoremap <A-Up> :m-2<CR>==
nnoremap <A-Left> <<
nnoremap <A-Right> >>
inoremap <A-Down> <Esc>:m+<CR>==gi
inoremap <A-Up> <Esc>:m-2<CR>==gi
inoremap <A-Left> <Esc><<`]a
inoremap <A-Right> <Esc>>>`]a
vnoremap <A-Down> :m'>+<CR>gv=gv
vnoremap <A-Up> :m-2<CR>gv=gv
vnoremap <A-Left> <gv
vnoremap <A-Right> >gv
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

" tabs
nnoremap <f10> :tabnew<cr>
nnoremap <f11> :tabp<cr>
nnoremap <f12> :tabn<cr>

" explorer mappings
nnoremap <f1> :NERDTreeTabsToggle<cr>
nnoremap <f2> :<C-u>Unite -no-split -buffer-name=buffers buffer<CR>
nnoremap <f3> :TagbarToggle<cr>
nnoremap <f4> :<C-u>Unite -no-split -buffer-name=files buffer file_mru bookmark file_rec/async<CR>

" Yank from HEAD (aka per-line checkout from HEAD)
nnoremap <silent> <Leader>Y :exe 'norm! 0C'.system('git blame -pL'.line('.').',+1 HEAD '.expand('%').'<Bar>tail -n1 <Bar>cut -c2-<Bar>tr -d "\n"')<CR>0

" make <c-l> clear the highlight as well as redraw
nnoremap <C-L> :nohls<CR><C-L>
inoremap <C-L> <C-O>:nohls<CR>

" Space toggle folding when cursor is in a fold
" http://vim.wikia.com/wiki/Folding
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <C-Space> zf

" map Q to something useful
noremap Q gq

" map W to w since it's a common typo
command! W :w

" make Y consistent with C and D
nnoremap Y y$

" U = redo (I never use the undo-line stuff anyway)
nnoremap U <C-r>

" C-u = Gundo tree
nnoremap <C-u> :GundoToggle<CR>

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

" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
" hacks from above (the url, not jesus) to delete fugitive buffers when we
" leave them - otherwise the buffer list gets poluted
"
" add a mapping on .. to view parent tree
autocmd BufReadPost fugitive://* set bufhidden=delete
autocmd BufReadPost fugitive://*
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

" watch .vimrc for changes and reload
" http://superuser.com/a/417997
"augroup myvimrc
"    au!
"    au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
"augroup END

