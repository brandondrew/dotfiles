#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 1
fi

# if we are running a bare go.sh
if [ ! -r README ] ; then
    apt-get install git-core
    git clone git@github.com:technomancy/dotfiles.git ~/dotfiles
    chown -R phil ~/dotfiles
    exec ~/dotfiles/bin/init/go.sh
fi

if [ ! -r /etc/apt/sources.list.orig ] ; then
    cp /etc/apt/sources.list /etc/apt/sources.list.orig
    # Enable all disabled sources
    sed -i -e "s/# deb/deb/g" /etc/apt/sources.list
    # We don't want backports though
    sed -i -e "s/^.*backports.*$//g" /etc/apt/sources.list
    # no thanks, cdrom
    sed -i -e "s/^.*cdrom.*$//g" /etc/apt/sources.list

    apt-get update
fi

# Fie upon and hence with you!
apt-get remove app-install-data-commercial

# get the minimum to bootstrap
apt-get install git-core git-svn zile build-essential bison subversion autoconf
apt-get build-dep ruby-full emacs-snapshot w3m-el libruby1.8
mkdir ~/src

svn co http://svn.ruby-lang.org/repos/ruby/branches/ruby_1_8_6/ ~/src/ruby1.8
cd ~/src/ruby1.8 && autoconf && ./configure && make && make install

# Gems! from trunk, because we're crazy.
git svn clone svn+ssh://technomancy@rubyforge.org/var/svn/rubygems/trunk ~/src/rubygems
ruby ~/src/rubygems/setup.rb

# gotta have my remote X!
sed -i s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf

# don't write atimes
chattr +A /

chown -R phil ~

exit 0
