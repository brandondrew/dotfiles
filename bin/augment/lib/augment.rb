#!/usr/bin/env ruby

$LOAD_PATH << File.dirname(__FILE__)
$LOAD_PATH << File.dirname(__FILE__) + '/augmentors'

require 'optparse'
require 'rubygems'
require 'active_support' # just for String#constantize for now
require 'yaml'
require 'json'

require 'layer'
require 'string_extensions'

Options = {:output => 'ansi'}
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
    end
    
    def augment(file)
      # output metadata upon request
      Layer.layers[file].inject(File.read(file)){ |output, layer| layer.render(output) }
    end
  end

  AUGMENTORS = []
end

Dir.glob(File.dirname(__FILE__) + '/augmentors/*.rb').each do |file|
  require file
end

if __FILE__ == $0
  Augmentor.run(ARGV[0])
  puts Augmentor.augment(ARGV[0])
end
