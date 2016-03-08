# System-wide .bashrc file for interactive bash(1) shells.
if [ -z "$PS1" ]; then
   return
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

export PATH=/usr/local/bin:/usr/local/sbin:${HOME}/local/bin:${HOME}/.cabal/bin:${HOME}/APUE/bin:${HOME}/.local/bin:${HOME}/APUE/bin:${PATH}


## setting for rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

########################
## aliases
########################

# alias ls='ls --color=auto'

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
# export GOROOT=`go env GOROOT`
# export GOPATH=${HOME}/go
# export PATH=$PATH:$GOROOT/bin

# decided not to use homebrew
export GOPATH=${HOME}/go
export PATH=$PATH:/usr/local/go/bin:${GOPATH}/bin

## python autoenv
source /usr/local/opt/autoenv/activate.sh

#####################
# use MacVim as vim #
#####################

systemName="$(uname -a| cut -d' ' -f1)"
#alias vim="$(if [ "${systemName}" == "Darwin" ] ; then echo -n '/usr/local/bin/mvim -v --servername VIM ' ; else echo -n /usr/bin/vim ; fi)"
alias vim="$(if [ "${systemName}" = "Darwin" ] ; then echo -n '/usr/local/bin/vim' ; else echo -n /usr/bin/vim; fi) --servername VIM"
export EDITOR=vim
export VISUAL=vim

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

alias clean_containers='docker rm $(docker ps -a --filter="dangling=true"| tail -n+2| awk "{print \$1}")'
alias clean_images='docker rmi $(docker images --filter="dangling=true"|tail -n+2 |awk "{print \$3}")'

# The next line updates PATH for the Google Cloud SDK.
source ~/google-cloud-sdk/path.bash.inc

# The next line enables shell command completion for gcloud.
source ~/google-cloud-sdk/completion.bash.inc
