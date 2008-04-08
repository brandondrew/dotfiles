#!/bin/bash

xmodmap ~/.xmodmap
devilspie &
unclutter -idle 0.5 &
xbindkeys &
synergys &
~/src/mpd-stats/mpd-stats.pl &

exec gnome-session