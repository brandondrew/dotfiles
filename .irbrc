# *-ruby-mode-*

require 'irb/completion'
require 'rubygems'
require 'wirble'
require 'yaml'
# load "~/.emacs.d/irbsh/irbsh-lib.rb" if IRB.conf[:PROMPT_MODE] == :INF_RUBY

# start wirble (with color)
Wirble.init
Wirble.colorize if IRB.conf[:PROMPT_MODE] == :INF_RUBY

IRB.conf[:AUTO_INDENT]=true
