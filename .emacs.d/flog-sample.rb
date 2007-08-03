require 'rubygems'
require 'active_record'
class FlogSample < ActiveRecord::Base
end

class Flogger
  def simple
    return 1 + 2 * 3 / 4
  end

  def minorly_complex
    dumb_hash = Hash.new
    [:foo, :bar, :baz].each do |meta_syntactic_var|
      dumb_hash[meta_syntactic_var] = 
        meta_syntactic_var.to_s.gsub(/a/, 'aaa')
    end

    dumb_hash.each do |k, v|
      puts "What have we here?"
      puts v[0 .. 1]
    end

    FlogSample.find(:all).map(&:to_s).map{ |s| puts s }
  end
end
