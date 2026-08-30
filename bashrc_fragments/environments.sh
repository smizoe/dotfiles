export PATH="${PATH}:/usr/local/bin:/usr/local/sbin:$(python3 -c "import site; print(site.USER_BASE)")/bin:${HOME}/dotfiles/local/bin:${HOME}/.cabal/bin:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/.npm-packages/bin"

########################
## postgresql

export PGDATA=/usr/local/var/postgres

########################
## setting for less

export LESS=-iR
export LESSGLOBALTAGS=global
## it does not seem possible to pass options to pygmentize through LESSCOLORIZER
## use pygmentize directly instead
export LESSOPEN='| file=%s; if [[ -e "${file}" && $(pygmentize -N "$file") = "text" ]] ; then lesspipe.sh "$file" ; else pygmentize -g -O style=emacs "$file"; fi'

#############################################
## settings for go installed with homebrew ##
#############################################
if [ "${SYSTEM_NAME}" == "Darwin" ] ; then
  export GOROOT=`go env GOROOT`
  export PATH=$PATH:$GOROOT/bin
else
  export PATH=$PATH:/usr/local/go/bin:${HOME}/go/bin
fi


export EDITOR="em -nw"
export VISUAL="em -a emacs"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
