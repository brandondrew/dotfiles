!!Will the original author of this code please stand up?
##ho_enumerable.rb
require 'delegate'

module HOM
  class HigherOrderMessage
    def HigherOrderMessage.is_vital(method)
      return method =~ /__(.+)__|method_missing/
    end
    
    for method in instance_methods
      undef_method(method) unless is_vital(method)
    end
    
    def initialize(handler)
      @handler = handler
    end
  end
  
  class Do < HigherOrderMessage
    def method_missing(id, *args)
      @handler.each {|e| e.__send__(id,*args)}
      return nil
    end
  end
  
  class Are < HigherOrderMessage
    def method_missing(id, *args)
      return @handler.apply {|e| e.__send__(id,*args)}
    end
  end
  
  class AreNot < HigherOrderMessage
    def method_missing(id, *args)
      return @handler.apply {|e| not e.__send__(id,*args)}
    end
  end
  
  class Have < HigherOrderMessage
    def method_missing(id, *args)
      return ResultMatcher.new(@handler,id,args)
    end
  end
  
  class ResultMatcher < HigherOrderMessage
    def initialize(handler, method, args)
      super(handler)
      @method = method
      @args = args
    end
    
    def method_missing(id, *args)
      @handler.apply do |e|
	result = e.__send__(@method,*@args)
	result.__send__(id,*args)
      end
    end
  end
  
  class Sort < HigherOrderMessage
    def method_missing(id, *args)
      return @handler.sort do |e1, e2|
	v1 = e1.__send__(id,*args)
	v2 = e2.__send__(id,*args)
	v1 <=> v2
      end
    end
  end
  
  class ReverseSort < HigherOrderMessage
    def method_missing(id, *args)
      return @handler.sort do |e1, e2|
	v1 = e1.__send__(id,*args)
	v2 = e2.__send__(id,*args)
	v2 <=> v1
      end
    end
  end
  
  class Extract < HigherOrderMessage
    def method_missing(id, *args)
      return @handler.map {|e| e.__send__(id, *args)}
    end
  end
  
  class Collator
    def initialize(receiver)
      @receiver = receiver
    end
    
    def are
      return HOM::Are.new(self)
    end
    
    def are_not
      return HOM::AreNot.new(self)
    end
    
    def have
      return HOM::Have.new(self)
    end
  end
  
  class That < Collator
    def apply(&block)
      return @receiver.select(&block)
    end
  end
  
  class All < Collator
    def apply(&block)
      return @receiver.all?(&block)
    end
  end
  
  class Any < Collator
    def apply(&block)
      return @receiver.any?(&block)
    end
  end

# under Rails 1.2.1, I get errors trying to render the template error message if I have this defined
#  class Sum < HigherOrderMessage
#    def method_missing(id, *args)
#      total = 0
#      @handler.each {|e| total = total + e.__send__(id, *args) }
#      return total
#    end
#  end
end

module Enumerable
  def do
    return HOM::Do.new(self)
  end
  
  def that
    return HOM::That.new(self)
  end
  
  def all
    return HOM::All.new(self)
  end
  
  def any
    return HOM::Any.new(self)
  end
  
  def in_order_of
    return HOM::Sort.new(self)
  end
  
  def in_reverse_order_of
    return HOM::ReverseSort.new(self)
  end
  
  def as(a_class)
    return collect {|e| a_class.new(e)}
  end
  
  def extract
    return HOM::Extract.new(self)
  end
  
# under Rails 1.2.1, I get errors trying to render the template error message if I have this defined  
#  def sum
#    return HOM::Sum.new(self)
#  end
end