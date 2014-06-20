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
- (not sure if below lines are needed)
- mkdir build && cd build
- cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
- make ycm_support_libs

Optional powerline fonts https://powerline.readthedocs.org/en/latest/installation/linux.html#font-installation

Vim keys:
- F1 = NERDTree
- F2 = Unite buffers
- F3 = Tagbar
- F4 = Unite
- F6 = Toggle GoldenView auto resize
- F10 = new tab
- F11 = previous tab
- F12 = next tab
- ,? = Unite help
- s = EasyMotion search
- \hjkl = EasyMotion move
- C-arrows = navigate splits
- A-Left/Right = indent
- A-Up/Down = drag line
- U = redo
- C-u = Gundo
- ff = toggle fold
- Enter = follow link (help only)
- Backspace = go back from link (help only)

Vim commands:
- ; = :
- :W = :w
- :w!! = sudo :w
- \Y = checkout current line from git HEAD
