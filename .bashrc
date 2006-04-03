# .bashrc

# User specific aliases and functions

[ "$EMACS" == "t" ] || alias ls="ls --color"

alias emac="emacs -nw -q --no-site-file"
alias vm="emacs --vm"
alias ri="ri -f ansi -T"
alias conkeror="firefox -chrome chrome://conkeror/content"

alias ll="ls -l"
alias la="ls -a"
alias less="less -R"
alias l="ls -la"
alias grep="grep --color=auto"
alias svnci="rake test && svn ci"

PATH=$PATH:/sbin:/usr/sbin:~/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin
export PATH

export SVN_EDITOR="emacs -nw -q --no-site-file"

PS1='\e[0;36m[\u@\h \w]\\$ \[\e[0;39m\]'

# mjolnir has a green prompt
if [ `hostname` = "mjolnir" ] ; then
  PS1='\e[32m[\u@\h \w]# \[\e[0;39m\]'
fi

# root has a red prompt
if [ `/usr/bin/whoami` = "root" ] ; then
  PS1='\e[31m[\u@\h \w]# \[\e[0;39m\]'
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

