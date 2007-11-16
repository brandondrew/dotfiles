#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 1
fi

# enable universe
cp /etc/apt/sources.list /etc/apt/sources.list.orig
sed -i -e "s/# deb/deb/g" /etc/apt/sources.list
apt-get update

# get the minimum
apt-get install git-core ruby zile

if [ -r install.rb ] ; then
    ruby install.rb
    su phil ./user-setup.sh
    exit 0
fi

# if we are running a bare go.sh
git clone git://git.technomancy.us/technomancy-init
cd technomancy-init
./go.sh
