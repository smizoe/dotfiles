export PATH="/usr/local/bin:/usr/local/sbin:${HOME}/dotfiles/local/bin:${HOME}/.cabal/bin:${HOME}/APUE/bin:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/APUE/bin:${PATH}"

export CPATH=${CPATH}:~/APUE/include
export LIBRARY_PATH=${LIBRARY_PATH}:~/APUE/lib
########################
## postgresql

export PGDATA=/usr/local/var/postgres

########################
## setting for less

export LESS=-iR

#############################################
## settings for go installed with homebrew ##
#############################################
if [ "${SYSTEM_NAME}" == "Darwin" ] ; then
  export GOROOT=`go env GOROOT`
  export GOPATH=${HOME}/go
  export PATH=$PATH:$GOROOT/bin
else
  export GOPATH=${HOME}/go
  export PATH=$PATH:/usr/local/go/bin:${GOPATH}/bin
fi


export EDITOR="em -nw"
export VISUAL="em -a emacs"
