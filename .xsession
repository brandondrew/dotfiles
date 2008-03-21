#!/bin/bash

xmodmap ~/.xmodmap
devilspie &
unclutter -idle 0.5 &
xbindkeys &

pidgin &

exec gnome-session