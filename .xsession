#!/bin/bash

xmodmap ~/.xmodmap
devilspie &
unclutter -idle 0.5 &
xbindkeys &
~/src/mpd-stats/mpd-stats.pl &
conkeror -daemon &
synergys &

exec gnome-session