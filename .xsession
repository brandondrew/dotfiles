#!/bin/bash

devilspie &
unclutter -idle 0.5 &
xbindkeys &
conkeror -daemon &

source ~/.profile

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

exec gnome-session

# gnome-settings-daemon &
# gnome-power-manager &
# nm-applet &
# exec sawfish
