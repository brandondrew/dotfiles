#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 0
fi

# add to /etc/apt/sources.list
# deb http://us.archive.ubuntu.com/ubuntu dapper universe main restricted multiverse
# deb http://security.ubuntu.com/ubuntu dapper-security universe multiverse

apt-get update

apt-get install epiphany-browser epiphany-extensions emacs21 subversion ruby1.8-dev sqlite3 libsqlite3-dev f-spot gmpc x2x gaim-otr gaim-encryption gaim-hotkeys synergy libsqlite3-ruby1.8 abiword gnumeric avahi-daemon bittorrent sbcl armagetron ekiga build-essential graphviz sshfs openssh-server mpg321 netcat nmap popularity-contest sawfish wmii rxvt-unicode sawfish-themes screen snes9x-x swig tidy unison unzip visualboyadvance w3m-img wmctrl workrave wxvlc xmms cvs xfonts-terminus namazu fetchmail libopenssl-ruby1.8 xulrunner zile mysql-server libmysql-ruby1.8 rdoc1.8 ri1.8 libnotify-bin ruby-gnome2 libgnome2-ruby rubygems

sed s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf.old > /etc/gdm/gdm.conf

gem install rails mongrel rcov tattle capistrano daemons cheat fastri hoe hpricot redcloth redgreen parsetree sqlite3-ruby termios tidy tzinfo ZenTest rspec rake vpim --include-dependencies