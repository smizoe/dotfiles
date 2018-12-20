########################
## aliases
########################

alias ls="$(if [ "${SYSTEM_NAME}" = "Darwin" ] ; then echo -n 'ls -G' ; else echo -n 'ls --color=auto'; fi)"
alias g='hub'
alias npm-exec='PATH=$(npm bin):$PATH'


alias vim="$(if [ "${SYSTEM_NAME}" = "Darwin" ] ; then echo -n '/usr/local/bin/vim' ; else echo -n /usr/bin/vim; fi) --servername VIM"
alias ed="em -nw"

alias clean_containers='docker rm $(docker ps -a --filter="status=exited"| tail -n+2| awk "{print \$1}")'
alias clean_images='docker rmi $(docker images --filter="dangling=true"|tail -n+2 |awk "{print \$3}")'

# ag for java project

alias jg='ag --ignore=build --ignore=target'

alias record='ffmpeg -r 30 -f avfoundation -i "1:0" -framerate 30 -af "highpass=f=200, lowpass=f=3000"'

alias csv2tsv='awk -v FPAT="([^,]*)|(\"[^\"]*\")" '"'BEGIN{ OFS=\"\t\"}{ for(i = 1; i <= NF; i++) { if (substr(\$i, 1, 1) == \"\\\"\") { len = length(\$i); \$i = substr(\$i, 2, len-2) } else {\$i = \$i} } ; print }'"

## enable several settings in arch linux
if [ "${SYSTEM_NAME}" != "Darwin" ] ; then
  alias open="xdg-open"
fi

# .net in linux; we need to chage TERM env. variable

for cmd in csharp dotnet
do
    alias "${cmd}"="TERM=xterm ${cmd}"
done
