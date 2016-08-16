function fish_greeting
end

function vim
  env SHELL=/bin/bash vim $argv
end

set -gx PATH /home/wamaral/bin /home/wamaral/.fzf/bin $PATH

# Fish git prompt
set __fish_git_prompt_show_informative_status 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream auto informative git
set __fish_git_prompt_showcolorhints 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '●'
set __fish_git_prompt_char_stagedstate '✓'
set __fish_git_prompt_char_untrackedfiles '?'
set __fish_git_prompt_char_stashstate '♽'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'

# shell
set -gx TERM xterm-256color
set -gx NVIM_TUI_ENABLE_TRUE_COLOR 1
set -gx COLORTERM 1

# aliases
#alias node='env NODE_NO_READLINE=1 rlwrap babel-node ~/.node_start.js'

# rbenv
#rbenv init - | source

# direnv
eval (direnv hook fish)

#
set -gx EDITOR vim
set -gx GROFF_NO_SGR 1
set -gx MANPAGER 'bash -c "vim -MRn -c \"set ft=man nomod nolist nospell nonu norelativenumber\" -c \"nm q :qa!<CR>\" -c \"nm <end> G\" -c \"nm <home> gg\"</dev/tty <(col -b)"'

