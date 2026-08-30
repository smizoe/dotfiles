# System-wide .bashrc file for interactive bash(1) shells.
if [ -z "$PS1" ]; then
   return
fi

BASHRC_REALPATH="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" && pwd)"

. "${BASHRC_REALPATH}/bashrc_fragments/environments.sh"

# If not running interactively, don't do anything other than setting up env. vars
[[ $- != *i* ]] && return
# return when the current shell is for a Zed task
# https://github.com/zed-industries/zed/issues/59542
[[ -n "$IS_ZED_TASK" ]] && return

. "${BASHRC_REALPATH}/bashrc_fragments/constants.sh"
. "${BASHRC_REALPATH}/bashrc_fragments/functions.sh"

# TMUX
if which tmux >/dev/null 2>&1; then
    #if not inside a tmux session
    if [ -z "$TMUX" -a -z "$INSIDE_EMACS" ] ; then
        # check if we would like to run bash with tmux
        if ask_run_with_tmux ; then
            # start a new session or attach to an existing session
            exec tmux new-session -A -s "$(get_tmux_id)"
        fi
    fi
fi

initialize

for fname in aliases keybinds
do
    . "${BASHRC_REALPATH}/bashrc_fragments/${fname}.sh"
done

if [ -f ~/.others.sh ] ; then
    source ~/.others.sh
fi
