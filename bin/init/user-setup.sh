#!/usr/bin/env bash

cd ~
#git clone ssh://dev.technomancy.us/git/dotfiles
ln -s .sshconfig .ssh/config
ln -s /usr/lib/ruby/gems/1.8/gems ~/gems

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