#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 1
fi

# Fie upon and hence with you!
apt-get remove app-install-data-commercial

# get the minimum to bootstrap
apt-get install git-core git-svn zile build-essential bison subversion autoconf ruby1.8 ri1.8 rdoc1.8 irb1.8 ruby1.8-dev
apt-get build-dep emacs-snapshot w3m-el
mkdir ~/src

# Gems! from trunk, because we're crazy.
git svn clone svn+ssh://technomancy@rubyforge.org/var/svn/rubygems/trunk ~/src/rubygems
ruby ~/src/rubygems/setup.rb

# gotta have my remote X!
sed -i s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf

# don't write atimes
chattr +A /

chown -R phil ~

exit 0
