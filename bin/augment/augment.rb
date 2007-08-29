#!/usr/bin/env ruby

require 'optparse'
require 'rubygems'
require 'active_support' # just for String#constantize for now
require 'yaml'

$LOAD_PATH << File.dirname(__FILE__)
require 'layer'
require 'string_extensions'

Options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: augment.rb [options]"

  opts.on("-o", "--output=OUTPUT_MODE", "Output to various formats. (emacs, html, etc.)") do |o|
    Options[:output] = o
  end
end.parse!

class Augmentor
  class << self
    def run(target)
      # gather metadata about file(s)
      Layer.new(target, (7 .. 11), 'red', 'you screwed up.')
      Layer.new(target, (5 .. 5), 'green', 'you rule.')
    end
    
    def augment(file)
      # output metadata upon request
      Layer.layers[file].inject(File.read(file)){ |output, layer| layer.render(output) }
    end
  end
end

Augmentor.run(ARGV[0])
puts Augmentor.augment(ARGV[0])
