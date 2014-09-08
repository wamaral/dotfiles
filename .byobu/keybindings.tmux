source /usr/share/byobu/keybindings/f-keys.tmux.disable \; display-message "Byobu F-keys: DISABLED [S-F12]"

bind-key -n F11 previous-window
bind-key -n F12 next-window
bind-key -n F7 swap-window -t :-1
bind-key -n F8 swap-window -t :+1

bind-key -n M-Up display-panes \; swap-pane -s :. -t :.- \; select-pane -t :.-
bind-key -n M-Down display-panes \; swap-pane -s :. -t :.+ \; select-pane -t :.+

bind-key -n M-S-Up resize-pane -U
bind-key -n M-S-Down resize-pane -D
bind-key -n M-S-Left resize-pane -L
bind-key -n M-S-Right resize-pane -R

bind-key -n S-F12 source /usr/share/byobu/keybindings/f-keys.tmux.disable \; display-message "Byobu F-keys: DISABLED"
bind-key -n M-F12 source /usr/share/byobu/keybindings/mouse.tmux.enable

unbind-key -n C-a
unbind-key -n C-o
set -g prefix ^O
set -g prefix2 ^O
bind o send-prefix
