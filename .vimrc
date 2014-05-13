set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
"Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
"Plugin 'L9'
" Git plugin not hosted on GitHub
"Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
"Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
"Plugin 'user/L9', {'name': 'newL9'}

Plugin 'mileszs/ack.vim' " Vim plugin for the Perl module / CLI script 'ack'
Plugin 'bling/vim-airline' " lean & mean status/tabline for vim that's light as air
Plugin 'godlygeek/csapprox' " Make gvim-only colorschemes work transparently in terminal vim
Plugin 'Raimondi/delimitMate' " provides insert mode auto-completion for quotes, parens, brackets, etc.
Plugin 'tpope/vim-fugitive' " A Git wrapper so awesome, it should be illegal
Plugin 'mattn/gist-vim' " vimscript for gist
Plugin 'gregsexton/gitv' " gitk for Vim
Plugin 'scrooloose/nerdcommenter' " A plugin that allows for easy commenting of code for many filetypes
Plugin 'Valloric/YouCompleteMe' " A code-completion engine for Vim
Plugin 'scrooloose/nerdtree' " A tree explorer plugin for navigating the filesystem           <F1>
Plugin 'jistr/vim-nerdtree-tabs' " NERDTree and tabs together in Vim, painlessly
Plugin 'jlanzarotta/bufexplorer' " BufExplorer Plugin for Vim                                 <F2>
Plugin 'majutsushi/tagbar' " Vim plugin that displays tags in a window, ordered by scope      <F3>
Plugin 'scrooloose/syntastic' " Syntax checking hacks for vim
Plugin 'SirVer/ultisnips' " The ultimate snippet solution for Vim
Plugin 'altercation/vim-colors-solarized' " precision colorscheme for the vim text editor
Plugin 'mattn/webapi-vim' " vim interface to Web API
Plugin 'jaredly/vim-debug' " A plugin for VIM that creates an Integrated Debugging Environment (PHP / Python)
Plugin 'airblade/vim-gitgutter' " A Vim plugin which shows a git diff in the gutter (sign column) and stages/reverts hunks.
Plugin 'kien/ctrlp.vim' " Fuzzy file, buffer, mru, tag, etc finder

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" syntax
syntax enable

" colors
set background=dark
colorscheme torte

" mine
set so=3 " number of screen lines to show around the cursor
set siso=7 " minimal number of columns to keep left and right of the cursor
set ss=1 " minimal number of columns to scroll horizontally
set nu " show the line number for each line
set sb " a new window is put below the current one
set spr " a new window is put right of the current one
set mouse=a " list of flags for using the mouse
set ttym=xterm2 " "xterm", "xterm2", "dec" or "netterm"; type of mouse
set ts=2 " number of spaces a <Tab> in the text stands for
set sw=2 " number of spaces used for each step of (auto)indent
set sta " a <Tab> in an indent inserts 'shiftwidth' spaces
set et " expand <Tab> to spaces in Insert mode
set ai " automatically set the indent of a new line
set si " do clever autoindenting
set cin " enable specific indenting for C code
set hi=1000 " how many command lines are remembered
set list " show <Tab> as ^I and end-of-line as $
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅ " list of strings used for list mode
set wim=list:longest " specifies how command line completion works (bash-like)
set wmnu " command-line completion shows a list of matches
set wig=*.o,*.obj,*~ " list of patterns to ignore files for file name completion
set formatoptions-=o " don't continue comments when o/O
set showtabline=2 " always show tab bar
set guioptions-=L " fix gvim resizing bug when opening tabs

" gvim font
if has('gui_running')
  set guifont=Inconsolata\ Medium\ 10
endif


" airline settings
set laststatus=2 " always show status line
let g:airline#extensions#tabline#enabled = 1 " Automatically displays tab line.
let g:airline#extensions#tabline#show_buffers = 0 " Don't show buffers (we use tabs)

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

" tabs
nnoremap <f8> :tabnew<cr>
nnoremap <f9> :tabp<cr>
nnoremap <f10> :tabn<cr>

"explorer mappings
nnoremap <f1> :NERDTreeTabsToggle<cr>
nnoremap <f2> :BufExplorer<cr>
nnoremap <f3> :TagbarToggle<cr>


"make <c-l> clear the highlight as well as redraw
nnoremap <C-L> :nohls<CR><C-L>
inoremap <C-L> <C-O>:nohls<CR>

"map Q to something useful
noremap Q gq

"make Y consistent with C and D
nnoremap Y y$


"jump to last cursor position when opening a file
"dont do it when writing a commit log entry
autocmd BufReadPost * call SetCursorPosition()
function! SetCursorPosition()
    if &filetype !~ 'svn\|commit\c'
        if line("'\"") > 0 && line("'\"") <= line("$")
            exe "normal! g`\""
            normal! zz
        endif
    end
endfunction

"spell check when writing commit logs
autocmd filetype svn,*commit* setlocal spell

"http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
"hacks from above (the url, not jesus) to delete fugitive buffers when we
"leave them - otherwise the buffer list gets poluted
"
"add a mapping on .. to view parent tree
autocmd BufReadPost fugitive://* set bufhidden=delete
autocmd BufReadPost fugitive://*
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

