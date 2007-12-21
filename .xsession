#!/bin/bash

devilspie &
unclutter -idle 0.5 &

gmpc &
pidgin &

exec gnome-session