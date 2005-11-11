# .bashrc

# User specific aliases and functions

[ "$EMACS" == "t" ] || alias ls="ls --color"

alias ll="ls -l"
alias la="ls -a"
alias less="less -R"
alias l="ls -la"
alias grep="grep --color=auto"

PATH=$PATH:/sbin:/usr/sbin:~/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin
export PATH

export SVN_EDITOR=zile

PS1='\e[0;36m[\u@\h \w]\\$ \[\e[0;39m\]'

if [ `/usr/bin/whoami` = "root" ] ; then
  PS1='\e[31m[\u@\h \w]# \[\e[0;39m\]'
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

