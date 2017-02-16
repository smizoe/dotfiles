bind -x '"\C-r": peco_select_history "${READLINE_LINE}"'
bind -x '"\er": source ~/.bashrc'
bind -x '"\C-s": em -e '"'"'(progn (setq-local helm-dash-docsets (helm-dash-installed-docsets))(helm-dash))'"'"''
