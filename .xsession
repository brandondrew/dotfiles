#!/bin/bash

devilspie &
unclutter -idle 0.5 &
xbindkeys &
conkeror -daemon &

exec gnome-session