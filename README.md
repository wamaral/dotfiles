dotfiles
========

Requires https://github.com/junegunn/vim-plug
- mkdir -p ~/.vim/autoload
- curl -fLo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
- :PlugInstall

Requires https://github.com/Valloric/YouCompleteMe
- apt-get install build-essential cmake python-dev libclang-dev
- cd ~/.vim/bundle/YouCompleteMe
- git submodule update --init --recursive
- ./install.sh
- (not sure if below lines are needed)
- mkdir build && cd build
- cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
- make ycm_support_libs

Optional powerline fonts https://powerline.readthedocs.org/en/latest/installation/linux.html#font-installation

Help on runtime:
- ,? = Unite
- -? = Custom keys
- -?? = F keys

Vim keys:
- F1 = vimfiler
- F2 = Tagbar
- F3 = Unite buffers
- F4 = Unite files
- F5 = Toggle buffer with main split (GoldenView)
- S-F5 = GoldenView split
- F6 = GoldenView resize current buffer
- S-F6 = Toggle GoldenView auto resize
- F10 = new tab
- F11 = previous tab
- F12 = next tab
- -d = delete buffer
- C-j = next buffer
- C-k = prev buffer
- s = EasyMotion search
- -hjkl = EasyMotion move
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
- -Y = checkout current line from git HEAD
