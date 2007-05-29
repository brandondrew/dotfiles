# .bashrc

# User specific aliases and functions

[ "$EMACS" == "t" ] || alias ls="ls --color"

alias emac="emacs -nw -q --no-site-file"
alias ri="ri -f ansi -T"
alias devlog="tail -F log/development.log"

#alias cd="cd \!*; ls"
alias ll="ls -l -h"
alias la="ls -a"
alias less="less -R"
alias l="ls -la"
alias grep="grep --color=auto"
alias svnci="rake test && svn ci"

alias svnpx="svn up; svn up -r 656 config/database.yml"

alias sapti="sudo apt-get install"
alias saptr="sudo apt-get remove"
alias saptu="sudo apt-get upgrade"
alias saptd="sudo apt-get update"
alias saptc="apt-cache search"
alias sapts="apt-cache show"

alias xmlcurl="curl -H Accept:text/xml"

alias ml="ls ~/music"

tmpit () { cp "$*" ~/mjolnir/apps/technomancy/public/tmp/; }

export SVN_EDITOR="emacs -nw -q --no-site-file"

complete -C ~/bin/rake-complete.rb -o default rake

# blue by default
PS1='\e[0;36m[\u@\h \w]\\$ \[\e[0;39m\]'

# mjolnir has a green prompt
if [ `hostname` = "mjolnir" ] ; then
  PS1='\e[32m[\u@\h \w]# \[\e[0;39m\]'
fi

# paxtel has purple
if [ `hostname` = "www.paxtel.com" ] ; then
  PS1='\e[35m[\u@\h \w]# \[\e[0;39m\]'
fi

# root has a red prompt
if [ `/usr/bin/whoami` = "root" ] ; then
  PS1='\e[31m[\u@\h \w]# \[\e[0;39m\]'
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# eterm-color causes screen problems
if [ $TERM = "eterm-color" ] ; then
    TERM=xterm
fi

#MPD_HOST=mjolnir
#MPD_PORT=6600
#if [ `ssh 192.168.1.2 hostname` != "mjolnir" ] ; then
#    MPD_HOST=localhost
#fi