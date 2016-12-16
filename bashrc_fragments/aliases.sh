########################
## aliases
########################

alias ls="$(if [ "${SYSTEM_NAME}" = "Darwin" ] ; then echo -n 'ls -G' ; else echo -n 'ls --color=auto'; fi)"
alias g='git'
alias npm-exec='PATH=$(npm bin):$PATH'


alias vim="$(if [ "${SYSTEM_NAME}" = "Darwin" ] ; then echo -n '/usr/local/bin/vim' ; else echo -n /usr/bin/vim; fi) --servername VIM"
alias ed="em -nw"

alias clean_containers='docker rm $(docker ps -a --filter="status=exited"| tail -n+2| awk "{print \$1}")'
alias clean_images='docker rmi $(docker images --filter="dangling=true"|tail -n+2 |awk "{print \$3}")'

# ag for java project

alias jg='ag --ignore=build --ignore=target'

## enable several settings in arch linux
if [ "${SYSTEM_NAME}" != "Darwin" ] ; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  export GTAGSCONF=/usr/share/gtags/gtags.conf
fi
