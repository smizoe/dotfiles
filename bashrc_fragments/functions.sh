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
function ask_run_without_tmux {
    local y_or_n
    echo "Do you want to run bash without tmux? (y or n)" >&2
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
