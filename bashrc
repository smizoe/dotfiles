# System-wide .bashrc file for interactive bash(1) shells.
if [ -z "$PS1" ]; then
   return
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

BASHRC_REALPATH="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" && pwd)"

. "${BASHRC_REALPATH}/bashrc_fragments/functions.sh"

# TMUX
if which tmux >/dev/null 2>&1; then
    #if not inside a tmux session
    if [ -z "$TMUX" ] ; then
        # check if we would like to run bash with tmux
        if ask_run_with_tmux ; then
            # start a new session or attach to an existing session
            exec tmux new-session -A -s "$(get_tmux_id)"
        fi
    fi
fi

# Make bash check its window size after a process completes
shopt -s checkwinsize

## stop using ctrl-s/ctrl-q as a control flow key so that we can use 'bind -x "\C-s": some command'
stty -ixon -ixoff

## make terminal sane after a command
SAVE_TERM="$(stty -g)"
PROMPT_COMMAND="log_bash_cmd '${HOME}/logs/bash/$(hostname)_$(date -u +'%Y-%m-%d').log'
stty ${SAVE_TERM}
$PROMPT_COMMAND"

# Tell the terminal about the working directory at each prompt.
if [ "$TERM_PROGRAM" == "Apple_Terminal" ] && [ -z "$INSIDE_EMACS" ]; then
    update_terminal_cwd() {
        # Identify the directory using a "file:" scheme URL,
        # including the host name to disambiguate local vs.
        # remote connections. Percent-escape spaces.
        local SEARCH=' '
        local REPLACE='%20'
        local PWD_URL="file://$HOSTNAME${PWD//$SEARCH/$REPLACE}"
        printf '\e]7;%s\a' "$PWD_URL"
    }
    PROMPT_COMMAND="update_terminal_cwd; $PROMPT_COMMAND"
fi

###################################
## the followings are user-defined.


# bash_completion
BASH_COMPLETION=/usr/local/etc/bash_completion
BASH_COMPLETION_DIR=/usr/local/etc/bash_completion.d
#BASH_COMPLETION_COMPAT_DIR=/usr/local/etc/bash_completion.d

BASH_COMPLETION_FILES=(/usr/local/etc/bash_completion /usr/share/git/completion/git-prompt.sh /usr/share/git/completion/git-completion.bash)
for file in ${BASH_COMPLETION_FILES[@]}
do
    if [ -f "${file}" ]; then
        . "${file}"
    fi
done


for fname in environments constants aliases keybinds
do
    . "${BASHRC_REALPATH}/bashrc_fragments/${fname}.sh"
done



## setting for rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi



####################
## show git branch

GIT_PS1_SHOWDIRTYSTATE=1
PS1="\[\$(color_from_status \$?)\]\u@\h\[${COLOR_NC}\] [\$(__git_ps1 \"(%s) \")\w]\\$ "


## python autoenv
source /usr/local/opt/autoenv/activate.sh


# The next line updates PATH for the Google Cloud SDK.
source ~/google-cloud-sdk/path.bash.inc

# The next line enables shell command completion for gcloud.
source ~/google-cloud-sdk/completion.bash.inc

__git_complete g __git_main
