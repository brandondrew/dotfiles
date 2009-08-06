#!/bin/bash

# devilspie &
# unclutter -idle 0.5 &
xbindkeys &
~/src/js/conkeror/contrib/run-conkeror -daemon &

xrdb -merge ~/.Xdefaults
source ~/.profile

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

pidgin &

exec gnome-session

#gnome-settings-daemon &
#gnome-power-manager &
#nm-applet &
#exec awesome
