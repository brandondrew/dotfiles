# -*-ruby-*-

require 'irb/completion'
require 'rubygems'
require 'wirble'
require 'yaml'

# start wirble (with color)
Wirble.init
# Wirble.colorize unless IRB.conf[:PROMPT_MODE] == :INF_RUBY

def decolor
  Wirble::Colorize.disable
  "Colorization disabled."
end

IRB.conf[:AUTO_INDENT]=true
