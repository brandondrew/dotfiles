#!/bin/bash

if [ `/usr/bin/whoami` != "root" ] ; then
    echo "You must be root."
    exit 0
fi

# add to /etc/apt/sources.list
# deb http://us.archive.ubuntu.com/ubuntu $RELEASE universe main restricted multiverse
# deb http://security.ubuntu.com/ubuntu $RELEASE-security universe multiverse

apt-get update

# core
apt-get install subversion ruby1.8-dev sqlite3 libsqlite3-dev libsqlite3-ruby1.8 build-essential sshfs openssh-server screen swig unzip cvs libopenssl-ruby1.8 zile mysql-server libmysql-ruby1.8 rdoc1.8 ri1.8 console-terminus

# desktop-specific
apt-get install epiphany-browser epiphany-extensions emacs21 f-spot gmpc x2x gaim-otr gaim-encryption gaim-hotkeys synergy abiword gnumeric avahi-daemon bittorrent sbcl armagetron ekiga graphviz  mpg321 netcat nmap popularity-contest sawfish wmii rxvt-unicode sawfish-themes snes9x-x tidy unison visualboyadvance w3m-img wmctrl workrave wxvlc xmms xfonts-terminus namazu fetchmail xulrunner libnotify-bin ruby-gnome2 libgnome2-ruby mplayer konqueror k3b lame gstreamer0.10-plugins-ugly-multiverse gstreamer0.10-fluendo-mp3 libgnome2-ruby ruby-gnome2 svk service-discovery-applet avahi-discover avahi-utils unclutter

sed -i s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf

gem install -y rails mongrel rcov tattle capistrano daemons cheat fastri hoe hpricot redcloth redgreen parsetree sqlite3-ruby termios tidy tzinfo ZenTest rspec rake vpim camping json rcodetools