#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 1
fi

if [ ! -r /etc/apt/sources.list.orig ] ; then
    # enable universe
    cp /etc/apt/sources.list /etc/apt/sources.list.orig
    # Enable all disabled sources
    sed -i -e "s/# deb/deb/g" /etc/apt/sources.list
    # We don't want backports though
    sed -i -e "s/^.*backports.*$//g" /etc/apt/sources.list
    sed -i -e "s/^.*cdrom.*$//g" /etc/apt/sources.list
    apt-get update
fi

# get the minimum
apt-get install git-core ruby ruby1.8 zile

if [ ! -r install.rb ] ; then
  # if we are running a bare go.sh
  cd ~
  git clone git@github.com:technomancy/dotfiles.git dotfiles
  chown -R $USER dotfiles
  cd dotfiles/bin/init
fi

ruby install.rb
chown -R $USER $HOME
sudo -u $USER ruby user-setup.rb # TODO: this breaks... huh?
exit 0
