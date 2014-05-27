dotfiles
========

Requires https://github.com/gmarik/Vundle.vim
- mkdir -p ~/.vim/bundle
- git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
- :PluginInstall

Requires https://github.com/Valloric/YouCompleteMe
- apt-get install build-essential cmake python-dev
- cd ~/.vim/bundle/YouCompleteMe
- ./install.sh --clang-completer

Optional powerline fonts https://powerline.readthedocs.org/en/latest/installation/linux.html#font-installation

Vim keys:
- F1 = NERDTree
- F2 = BufExplorer
- F3 = Tagbar
- F4 = CtrlP
- F6 = new tab (console)
- F7 = previous tab (console)
- F8 = next tab (console)
- F10 = new tab (gui)
- F11 = previous tab (gui)
- F12 = next tab (gui)
- C-arrows = navigate splits (gui)
- A-Left/Right = indent (gui)
- A-Up/Down = drag line (gui)
- U = redo
- C-u = Gundo
- Space = toggle fold
- Enter = follow link (help only)
- Backspace = go back from link (help only)

Vim commands:
- ; = :
- :W = :w
- :w!! = sudo :w
- \Y = checkout current line from git HEAD
