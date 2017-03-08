bind -x '"\C-r": peco_select_history "${READLINE_LINE}"'
bind -x '"\er": source ~/.bashrc'
bind -x '"\eh": em -nw -e '"'"'(progn (setq-local helm-dash-docsets (helm-dash-installed-docsets))(helm-dash))'"'"''
bind -x '"\C-s": em -nw -e "(helm-ag (expand-file-name \"${READLINE_LINE}\"))"'
