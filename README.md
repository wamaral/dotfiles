dotfiles
========

Requires https://github.com/Shougo/neobundle.vim
- curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh | sh
- :NeoBundleInstall
- :Unite neobundle/install (if unite is installed)

Requires https://github.com/Valloric/YouCompleteMe
- apt-get install build-essential cmake python-dev libclang-3.3-dev
- cd ~/.vim/bundle/YouCompleteMe
- git submodule update --init --recursive
- ./install.sh --clang-completer --system-libclang

Optional powerline fonts https://powerline.readthedocs.org/en/latest/installation/linux.html#font-installation

Vim keys:
- F1 = NERDTree
- F2 = BufExplorer
- F3 = Tagbar
- F4 = Unite
- F10 = new tab
- F11 = previous tab
- F12 = next tab
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
