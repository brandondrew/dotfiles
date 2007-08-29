require 'optparse'
require 'rubygems'
require 'active_support' # just for String#constantize for now


Options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: augment.rb [options]"

  opts.on("-o", "--output=OUTPUT_MODE", "Output to various formats. (emacs, html, etc.)") do |o|
    Options[:output] = o
  end
end.parse!

class Augmentor
  class << self
    def run
    end
    
    def augment(file)
      Layer.find_by_file(file).map{ |l| l.render }
    end
  end
end

class Layer
  attr_accessor :lines, :color, :message

  @layers = {}
  
  def self.find_by_file(file)
    @layers.select{ |layer_file, layer| layer_file =~ file}
  end
  
  def self.layers; @layers; end
  
  def initialize(lines, color, message)
    @lines, @color, @message = [lines, color, message]
    self.class.layers << self
  end
  
  def render
    # for now just render for Emacs; add more output formats later.
    "(save-excursion
  (goto-line #{@lines.first})
  (let ((overlay-start (line-beginning-position)))
    (goto-line #{@lines.last})
    (let ((overlay (make-overlay overlay-start (line-end-position)) ))
    (overlay-put overlay 'face (background-color . #{@color}))
    (overlay-put overlay 'message \"#{@message}\"))))"
  end
end

augmentor = "#{options[:output].to_s.capitalize}Augmentor".constantize.run

while input = gets
  augmentor.augment(input)
end
