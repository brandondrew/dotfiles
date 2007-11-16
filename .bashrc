# .bashrc

# User specific aliases and functions
source ~/.profile

[ "$EMACS" == "t" ] || alias ls="ls --color"

export PAGER=less

alias emac="emacs -nw -q --no-site-file"
alias ri="ri -f ansi -T"
alias devlog="tail -F log/development.log"

alias ll="ls -l -h"
alias la="ls -a"
alias l="ls"
alias lla="ls -a -l"
alias grep="grep --color=auto"

alias sapti="sudo apt-get install"
alias saptr="sudo apt-get remove"
alias saptu="sudo apt-get upgrade"
alias saptd="sudo apt-get update"
alias saptc="apt-cache search"
alias sapts="apt-cache show"
alias pgrep="ps awx | grep"

alias xmlcurl="curl -H Accept:text/xml"
alias feedconsole="ssh deploy@central /apps/feederator/current/script/console production"

alias ml="ls ~/music"

complete -C ~/bin/rake-complete.rb -o default rake

export EDITOR=emacsclient

# prompt coloring
# see http://attachr.com/9288 for full-fledged craziness
if [ `/usr/bin/whoami` = "root" ] ; then
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