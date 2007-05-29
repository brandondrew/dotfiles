require 'irb/completion'
require 'rubygems'
require 'wirble'
require 'yaml'

# start wirble (with color)
Wirble.init
Wirble.colorize

IRB.conf[:AUTO_INDENT]=true