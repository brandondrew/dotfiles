export PATH=$PATH:/sbin:/usr/sbin:~/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin:/var/lib/gems/1.8/bin:/home/phil/.gem/ruby/1.8/bin
export EDITOR="emacsclient"
export JAVA_OPTS="-client"
export SAKE_UNSAFE="why-not"

if [ -r $HOME/bin/jdk1.6.0_02 ]; then
    export JAVA_HOME=$HOME/bin/jdk1.6.0_02
    export PATH=$JAVA_HOME/bin:$PATH
fi

export JRUBY_HOME=$HOME/bin/jruby-1.1.4
export PATH=$PATH:$JRUBY_HOME/bin

export CDPATH=.:~/src:~/work