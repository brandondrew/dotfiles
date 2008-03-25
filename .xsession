#!/bin/bash

export DMENU_FONT='-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-9'

xmodmap ~/.xmodmap
devilspie &
unclutter -idle 0.5 &
xbindkeys &
synergys &

exec gnome-session