export PATH="${PATH}:/usr/local/bin:/usr/local/sbin:$(python3 -c "import site; print(site.USER_BASE)")/bin:${HOME}/dotfiles/local/bin:${HOME}/.cabal/bin:${HOME}/APUE/bin:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/.npm-packages/bin"

export CPATH=${CPATH}:~/APUE/include
export LIBRARY_PATH=${LIBRARY_PATH}:~/APUE/lib
########################
## postgresql

export PGDATA=/usr/local/var/postgres

########################
## setting for less

export LESS=-iR
export LESSGLOBALTAGS=global
## it does not seem possible to pass options to pygmentize through LESSCOLORIZER
## use pygmentize directly instead
export LESSOPEN='| file=%s; if [[ $(pygmentize -N "$file") = "text" ]] ; then lesspipe.sh "$file" ; else pygmentize -g -O style=friendly "$file"; fi'

#############################################
## settings for go installed with homebrew ##
#############################################
if [ "${SYSTEM_NAME}" == "Darwin" ] ; then
  export GOROOT=`go env GOROOT`
  export PATH=$PATH:$GOROOT/bin
else
  export PATH=$PATH:/usr/local/go/bin:${GOPATH}/bin
fi


export EDITOR="em -nw"
export VISUAL="em -a emacs"

## dasht
export DASHT_DOCSETS_DIR=~/.local/share/Zeal/Zeal/docsets
