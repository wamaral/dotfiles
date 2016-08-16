function ca
  # command cat ~/.bookmarks | sort | fzf +m > /tmp/fzf.result
  command find /home/wamaral/dev -maxdepth 1 -type d | fzf +m > /tmp/fzf.result
  [ (cat /tmp/fzf.result | wc -l) -gt 0 ]
    and cd (cat /tmp/fzf.result)
  commandline -f repaint
  rm -f /tmp/fzf.result
end
