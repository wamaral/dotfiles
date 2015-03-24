source-file $BYOBU_PREFIX/share/byobu/profiles/tmux

source-file $BYOBU_PREFIX/share/byobu/keybindings/f-keys.tmux.disable

set-option -g bell-on-alert on
set-option -g display-panes-time 400


set-window-option -g window-status-bell-bg $BYOBU_DARK
set-window-option -g window-status-bell-fg $BYOBU_HIGHLIGHT
set-window-option -g window-status-bell-attr bold,reverse
set-window-option -g window-status-activity-bg $BYOBU_DARK
set-window-option -g window-status-activity-fg $BYOBU_ACCENT
set-window-option -g window-status-activity-attr none

set-window-option -g window-status-separator " | "
