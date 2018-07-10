#!/bin/bash
alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias lah='lla -h'

alias diff='diff --color=auto'

alias grep='grep --color=auto'

alias git-skip='git update-index --skip-worktree'

alias pac='pacaur --noedit'
alias paca='pac -Syuv'
alias ya='yaourt -Syuva'
alias yay='ya --noconfirm'

alias ghci='stack exec -- ghci'

alias timestamp="awk '{ print strftime(\"%H:%M:%S\"), \$0; fflush(); }'"
