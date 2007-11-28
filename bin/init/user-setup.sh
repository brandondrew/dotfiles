#!/usr/bin/env bash

cd ~

# we git cloned this in go.sh
mv dotfiles/* ~
mv dotfiles/.* ~

ln -s .sshconfig .ssh/config
ln -s /usr/lib/ruby/gems/1.8/gems ~/gems # elsewhere on other platforms

rm -rf Music
rm -rf Pictures
rm -rf Public
rm -rf Templates
rm -rf Video

# making Emacs
sudo apt-get build-dep emacs
mkdir ~/src
cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/cvsroot/emacs co emacs
cd emacs
./configure
make bootstrap
make
sudo make install

# CVS Screen has vertical-splits!
sudo apt-get build-dep screen
cd ~/src
cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/screen co screen
cd screen/src
./configure
make
sudo make install

# grab ruby-dbus
cd ~/src
svn co svn://trac.luon.net/ruby-dbus/trunk ruby-dbus
cd ruby-dbus
ruby setup.rb config
ruby setup.rb setup
sudo ruby setup.rb install

# mozlab and firebug installations must be by hand
wget http://getfirebug.com/releases/firebug1.0-current.xpi
firefox firebug1.0-current.xpi
rm firebug1.0-current.xpi

wget http://repo.hyperstruct.net/mozlab/mozlab-current.xpi
firefox mozlab-current.xpi
rm mozlab-current.xpi

# TODO: configure panel... how to do this programmatically?
# further configuration:
ruby ~/bin/gconf.rb