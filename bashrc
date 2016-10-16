# System-wide .bashrc file for interactive bash(1) shells.
if [ -z "$PS1" ]; then
   return
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


function join_by { local IFS="$1"; shift; echo "$*"; }

function array_contains {
## check if the 2nd to last argument (array) contains the 1st argument.
    for elem in "${@:2}"
    do
      [[ "${elem}" == "${1}" ]] && return 0
    done
    return 1
}

function get_tmux_id {
    local session_id
    local tmux_sessions
    local sessions_str
    tmux_sessions=($(tmux ls|cut -d : -f1))
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
    echo "${session_id}"
}

# TMUX
if which tmux >/dev/null 2>&1; then
    #if not inside a tmux session, start a new session or attach to an existing session
    if [ -z "$TMUX" ] ; then
      ID="$(get_tmux_id)"
      exec tmux new-session -A -s "${ID}"
    fi
fi

PS1='\h:\W \u\$ '
# Make bash check its window size after a process completes
shopt -s checkwinsize
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

## utility variables
systemName="$(uname -a| cut -d' ' -f1)"

# bash_completion
BASH_COMPLETION=/usr/local/etc/bash_completion
BASH_COMPLETION_DIR=/usr/local/etc/bash_completion.d
#BASH_COMPLETION_COMPAT_DIR=/usr/local/etc/bash_completion.d
if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    . /usr/share/git/completion/git-prompt.sh
fi

export PATH=/usr/local/bin:/usr/local/sbin:${HOME}/dotfiles/local/bin:${HOME}/.cabal/bin:${HOME}/APUE/bin:${HOME}/.local/bin:${HOME}/APUE/bin:${PATH}


## setting for rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

########################
## aliases
########################

alias ls="$(if [ "${systemName}" = "Darwin" ] ; then echo -n 'ls -G' ; else echo -n 'ls --color=auto'; fi)"
alias g='git'
alias npm-exec='PATH=$(npm bin):$PATH'

function __mk_r_project () {
    if [ $# -eq 0 ]; then
        "Usage: $(basename $0) dirname[...]"
    else
        for dir in "$@"
        do
            for subdir in queries R data image
            do
                mkdir -p ${dir}/${subdir}
            done
            touch ${dir}/plan.org
        done
    fi
}

alias mkRdir='__mk_r_project'


function __mk_python_project () {
  if [ $# -eq 0 ] ; then
        "Usage: $(basename $0) dirname[...]"
  else
      for dir in "$@"
      do
            for subdir in ${dir} venv data
            do
                    mkdir -p ${dir}/${subdir}
            done
            pyvenv ${dir}/venv
            echo -e "#! /usr/bin/env bash\nsource venv/bin/activate" > ${dir}/.env
            cat <<EOF > venv/package_requirements.txt
gnureadline==6.3.3
ipython==2.3.1
matplotlib==1.4.2
nose==1.3.4
numpy==1.9.1
pandas==0.15.1
patsy==0.3.0
pyparsing==2.0.3
python-dateutil==2.2
pytz==2014.9
scikit-learn==0.15.2
scipy==0.14.0
seaborn==0.5.0
six==1.8.0
statsmodels==0.6.0
EOF
      done
  fi
}

alias mkPydir='__mk_python_project'

export CPATH=${CPATH}:~/APUE/include
export LIBRARY_PATH=${LIBRARY_PATH}:~/APUE/lib

####################
## show git branch

GIT_PS1_SHOWDIRTYSTATE=1
PS1="\u@\h [\$(__git_ps1 \"(%s) \")\w]\\$ "

########################
## postgresql

export PGDATA=/usr/local/var/postgres

########################
## setting for less

export LESS=-iR

#############################################
## settings for go installed with homebrew ##
#############################################
if [ "${systemName}" == "Darwin" ] ; then
  export GOROOT=`go env GOROOT`
  export GOPATH=${HOME}/go
  export PATH=$PATH:$GOROOT/bin
else
  export GOPATH=${HOME}/go
  export PATH=$PATH:/usr/local/go/bin:${GOPATH}/bin
fi

## python autoenv
source /usr/local/opt/autoenv/activate.sh

#####################
# use MacVim as vim #
#####################

alias vim="$(if [ "${systemName}" = "Darwin" ] ; then echo -n '/usr/local/bin/vim' ; else echo -n /usr/bin/vim; fi) --servername VIM"
alias ed="em -nw"
export EDITOR="em -nw"
export VISUAL="em -a emacs"

## setting for vimcom
## set TERM variable appropriately so that vim chooses correct coloring scheme
if [ "$TERM" = "xterm" ] || [ "$TERM" = "xterm-256color" ]
then
    export TERM=xterm-256color
    export HAS_256_COLORS=yes
fi
if [ "$TERM" = "screen" ] && [ "$HAS_256_COLORS" = "yes" ]
then
    export TERM=screen-256color
fi

#export GTK_IM_MODULE=uim
#export QT_IM_MODULE=uim
#uim-xim &
#export XMODIFIERS=@im=uim

alias clean_containers='docker rm $(docker ps -a --filter="status=exited"| tail -n+2| awk "{print \$1}")'
alias clean_images='docker rmi $(docker images --filter="dangling=true"|tail -n+2 |awk "{print \$3}")'

# ag for java project

alias jg='ag --ignore=build --ignore=target'

# The next line updates PATH for the Google Cloud SDK.
source ~/google-cloud-sdk/path.bash.inc

# The next line enables shell command completion for gcloud.
source ~/google-cloud-sdk/completion.bash.inc

## enable several settings in arch linux
if [ "${systemName}" != "Darwin" ] ; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  export GTAGSCONF=/usr/share/gtags/gtags.conf
fi
