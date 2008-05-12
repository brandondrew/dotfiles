# -*-ruby-*-

require 'irb/completion'
require 'rubygems'
require 'wirble'
require 'yaml'

# start wirble (with color)
Wirble.init
# Wirble.colorize unless IRB.conf[:PROMPT_MODE] == :INF_RUBY

def ri(obj)
  puts "ri #{obj}"
end

# Inspecting really long strings causes inf-ruby to get really, really slow.
class String
  def inspect
    puts self
  end
end

IRB.conf[:AUTO_INDENT]=true
