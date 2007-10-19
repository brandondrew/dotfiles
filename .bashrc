# .bashrc

# User specific aliases and functions
source ~/.profile

[ "$EMACS" == "t" ] || alias ls="ls --color"
export EDITOR="emacs -Q -nw"
export PAGER=less

alias emac="emacs -nw -q --no-site-file"
alias ri="ri -f ansi -T"
alias devlog="tail -F log/development.log"

#alias cd="cd \!*; ls"
alias ll="ls -l -h"
alias la="ls -a"
alias l="ls -la"
alias grep="grep --color=auto"
alias svnci="rake test && svn ci"

alias svnpx="svn up; svn up -r 656 config/database.yml"
alias less="most"

alias sapti="sudo apt-get install"
alias saptr="sudo apt-get remove"
alias saptu="sudo apt-get upgrade"
alias saptd="sudo apt-get update"
alias saptc="apt-cache search"
alias sapts="apt-cache show"

alias xmlcurl="curl -H Accept:text/xml"
alias feedconsole="ssh deploy@central /apps/feederator/current/script/console production"

alias ml="ls ~/music"

function cdgem {
  cd /opt/local/lib/ruby/gems/1.8/gems/; cd `ls|grep $1|sort|tail -1`
}

tmpit () { cp "$*" ~/mjolnir/apps/technomancy/public/tmp/; }

export SVN_EDITOR="zile"

complete -C ~/bin/rake-complete.rb -o default rake

# prompt coloring
# see http://attachr.com/9288 for full-fledged craziness
if [ `hostname` = "mjolnir" ] ; then
  # mjolnir has a green prompt
  export PS1="\[\033[0;32m\]\u@\h \w \$ \[\033[0m\]"
elif [ `/usr/bin/whoami` = "root" ] ; then
  # root has a red prompt
  export PS1="\[\033[0;31m\]\u@\h \w \$ \[\033[0m\]"
elif [ `hostname` = "vannevar" -o `hostname` = "puyo" -o `hostname` = "imp" ] ; then
  export PS1="\[\033[0;36m\]\u@\h \w \$ \[\033[0m\]"
else
  # purple by default
  export PS1="\[\033[0;35m\]\u@\h \w \$ \[\033[0m\]"
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# eterm-color causes screen problems
if [ $TERM = "eterm-color" ] ; then
    TERM=xterm
fi