function initialize() {
    ## function used to initialize bash; we put all the steps that should be run only once.
    if [[ -z "${MY_INITIALIZED}" ]] ; then
        # Make bash check its window size after a process completes
        shopt -s checkwinsize

        ## stop using ctrl-s/ctrl-q as a control flow key so that we can use 'bind -x "\C-s": some command'
        stty -ixon -ixoff

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


        BASH_COMPLETION_FILES=(/usr/local/etc/bash_completion /usr/share/git/completion/git-prompt.sh /usr/share/git/completion/git-completion.bash)
        for file in ${BASH_COMPLETION_FILES[@]}
        do
            if [ -f "${file}" ]; then
                . "${file}"
            fi
        done

        ####################
        ## show git branch
        GIT_PS1_SHOWDIRTYSTATE=1
        if [ -z "${INSIDE_EMACS}" ] ; then
            PS1="\[\$(color_from_status \$?)\]\u@\h\[${COLOR_NC}\] [\$(__git_ps1 \"(%s) \")\w]\\$ "
            ## make terminal sane after a command
            SAVE_TERM="$(stty -g)"
            trap 'timer_start' DEBUG
            PROMPT_COMMAND="log_bash_cmd \"${HOME}/logs/bash/$(hostname)_\$(date -u +'%Y-%m-%d').log\"
                            stty ${SAVE_TERM}
                            $PROMPT_COMMAND
                            history -a
                            history -c
                            history -r
                            "
        else
            PS1="\u@\h [\$(__git_ps1 \"(%s) \")\w]\\$ "
        fi

        __git_complete g __git_main

        # The next line updates PATH for the Google Cloud SDK.
        if [ -f "${HOME}/dotfiles/google-cloud-sdk/path.bash.inc" ]; then source "${HOME}/dotfiles/google-cloud-sdk/path.bash.inc"; fi

        # The next line enables shell command completion for gcloud.
        if [ -f "${HOME}/dotfiles/google-cloud-sdk/completion.bash.inc" ]; then source "${HOME}/dotfiles/google-cloud-sdk/completion.bash.inc"; fi


        ## setting for rbenv
        if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
        #if which pyenv > /dev/null ; then eval "$(pyenv init -)"; fi

        MY_INITIALIZED=DONE
    fi
}

function color_from_status {
    local color
    if [ "$1" = "0" ] ; then
        color="${COLOR_GREEN}"
    elif [ "$1" = "1" ] ; then
        color="${COLOR_YELLOW}"
    else
        color="${COLOR_RED}"
    fi
    echo -e "${color}"
}

function join_by { local IFS="$1"; shift; echo "$*"; }

function array_contains {
## check if the 2nd to last argument (array) contains the 1st argument.
    for elem in "${@:2}"
    do
      [[ "${elem}" == "${1}" ]] && return 0
    done
    return 1
}

## this succeeds if user runs bash without tmux
function ask_run_with_tmux {
    local y_or_n
    echo "Do you want to run bash with tmux? (y or n)" >&2
    read y_or_n
    while [ "${y_or_n}" != "y" -a "${y_or_n}" != "n" ]
    do
        echo "Please answer with y or n." >&2
        read y_or_n
    done

    if [ "${y_or_n}" = "y" ] ; then
        return 0
    else
        return 1
    fi
}

function get_tmux_id {
    local session_id
    local tmux_sessions
    local sessions_str
    tmux_sessions=($(tmux ls|cut -d : -f1))
    if [ -z "${tmux_sessions}" ] ; then
        # use the default id, since there is no session
        session_id=0
    else
        sessions_str="$(join_by $'\n' ${tmux_sessions[@]})"
        local msg="$(cat <<EOF
The following tmux sessions are available:
${sessions_str}
Please select one of them (default: ${tmux_sessions[0]})
or enter a non-existing name to create a new session with the name.

name?:
EOF
)"
        echo -e "${msg}" >&2
        read session_id
        test -z "${session_id}" && session_id="${tmux_sessions[0]}"
    fi
    echo "${session_id}"
}


## select bash history based on the given argument
## this is intended to be used with 'bind -x' bash builtin.
function peco_select_history () {
    builtin history -n
    local selected="$(builtin history | fzf +s --reverse --tac --query "${1}" )"
    if [ -n "${selected}" ] ; then
        READLINE_LINE="$(echo "${selected}" | awk '{$1 = ""; print $0}')"
    fi
    return 0
}

function select_cd_directory () {
    local find
    if which gfind &> /dev/null ; then
        find=gfind
    else
        find=find
    fi
    2> /dev/null "${find}"  /* -regextype posix-extended \( -not -regex "/(projects|Users|home)/?[^.]*" -and -prune \) -o \( -regex ".*/(node_modules|build|vendor|\.git|\.gradle|\.idea|Library).*" -and -prune \) -o \( -type d -and -print \) | fzf --reverse --query "${*}"
}

function tms () {
    local buffer_name="$(</dev/null tmux lsb | fzf --reverse | awk -F ':' '{print $1}')"
    if [ -n "${buffer_name}" ] ; then
        tmux save-buffer -b "${buffer_name}" -
    fi
}

function tml () {
    local buffer_name="${1:-$(date +'%s')}"
    tmux load-buffer -b "${buffer_name}" -
}

function wp () {
    local target="$(select_cd_directory "$@")"
    if [ "$?" -eq 0 ] ; then
        cd "${target}"
    fi
}

## kill a pane based on the content of the file given as the 1st argument
## if the pane file does not exist or the file is empty, this function does nothing
## arguments:
##   $1: a file contains a pane id (%1, %10 etc.)
function kill_pane_from_pane_file() {
    local pane_file="$1"
    if [ -f "${pane_file}" ] ; then
        local pane_id="$(cat "${pane_file}")"
        if [[ -z "${pane_id}" ]] ; then
           echo "the pane file exists but nothing is in it." >&2
           return 0
        elif tmux list-pane | grep -q "${pane_id}" ; then
           tmux killp -t "${pane_id}"
           return 0
        fi
        echo "file ${pane_file} exists but the pane not found" >&2
    fi
    echo "regular file ${pane_file} does not exist" >&2
}

## given a line from a result of vimgrep as the 1st argument, get a glimpse of the file around the line.
## or if just a path (without colon in it) is given, open it for preview
## returns 0 if success, otherwise 1
## arguments:
## $1: a line from a result of vimgrep
## PANE_FILE: a lock file that contains which pane we use for glimpsing a file. if empty, we make a file in temporary directory.
function peco_glimpse_fn() {
    if [ -z "${TMUX:-}" ] ; then
        echo "peco-glimpse must run within tmux" >&2
        return 1
    fi

    local pane_file="${PANE_FILE:-${TMPDIR:-/tmp}/peco-glimpse-${PPID}.pane}"

    # close a pane if exists
    kill_pane_from_pane_file "${pane_file}"

    # split a new pane and preview
    local pane_file_fd
    exec {pane_file_fd}>"${pane_file}"
    flock -n "${pane_file_fd}"
    if [ $? -ne "0" ] ; then
        echo "failed to get flock: ${pane_file}" >&2
        exec {pane_file_fd}>&-
        exit 1
    fi

    local line="$1"
    local path="$(echo "${line}" | awk -F ':' '{print $1}' )"
    local lineno="$(echo "${line}" | awk -F ':' '{print $2}' )"
    if [ -f "${path}" ] ; then
        tmux split-window -v -l 20 "less +${lineno:-1} -w -j 10 '${path}'"
        tmux display -p '#{pane_id}' > "${pane_file}"
        tmux last-pane
    else
        echo "no such file: ${path}" >&2
    fi
    exec {pane_file_fd}>&-
    return 0
}

function print_tmux_info() {
    if [ -n "${TMUX}" ] ; then
      tmux display-message -p "tmux_host:#H
tmux_pane_id:#D
tmux_pane_index:#P
tmux_pane_title:#T
tmux_session_name:#S
tmux_window_flags:#F
tmux_window_index:#I
tmux_window_name:#W"
    fi
}

## see https://stackoverflow.com/questions/1862510/how-can-the-last-commands-wall-time-be-put-in-the-bash-prompt#answer-1862762
function timer_start() {
  _timer=${_timer:-$SECONDS}
}

function timer_stop() {
  timer_show=$(($SECONDS - $_timer))
  unset timer
}

function log_bash_cmd() {
    local pipe_status="${PIPESTATUS[*]}"
    local last_cmd="$(builtin history 1 | awk '{$1=""; print $0}')"
    timer_stop
    echo -n "unix_timestamp:$(date +%s)
PWD:${PWD}
OLDPWD:${OLDPWD}
PIPESTATUS:${pipe_status}
$(print_tmux_info)
tmux_socket:${TMUX}
last_command_elapsed_time:${timer_show}
CMD:${last_cmd}" | tr "\n" "\t" | ltsv2json >> "$1"
    echo >> "$1"
}

function ltsv2json() {
  cat "${@:-/dev/stdin}"| ruby -rjson -npe '$_ = JSON.generate($_.chomp.split("\t").map{|elem| elem.split(":",2) }.inject({}){|acc, pair| acc[pair[0]] = pair[1]; acc})'
}

export -f peco_glimpse_fn kill_pane_from_pane_file print_tmux_info
