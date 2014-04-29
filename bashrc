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

export PATH=/usr/local/bin:/usr/local/sbin:${HOME}/local/bin:${HOME}/.local/bin:${HOME}/APUE/bin:${PATH}


export EDITOR=emacsclient
export VISUAL=emacsclient

## setting for rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# alias ls='ls --color=auto'

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
## hive and java

export HIVE_HOME=/usr/local/Cellar/hive/0.12.0/libexec
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH


########################
## setting for less

LESS=-iN

#############################################
## settings for go installed with homebrew ##
#############################################
# export GOROOT=`go env GOROOT`
# export GOPATH=${HOME}/go
# export PATH=$PATH:$GOROOT/bin

# decided not to use homebrew
export GOPATH=${HOME}/go
export PATH=$PATH:/usr/local/go/bin:${GOPATH}/bin
