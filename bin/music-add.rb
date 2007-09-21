#!/usr/bin/env ruby

system 'mpc add ' + `find /home/phil/music -type d | cut -c 18- | dmenu`
