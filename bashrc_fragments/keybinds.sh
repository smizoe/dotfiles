if [ -z "${INSIDE_EMACS}" ] ; then
    bind -x '"\C-r": peco_select_history "${READLINE_LINE}"'
    bind -x '"\er": source ~/.bashrc'
    bind -x '"\C-s": elinks http://localhost:54321/'
fi
