# See http://redhanded.hobix.com/inspect/ideasForRuby20Veneer.html for more.
# The idea is to chisel off some of the extras we often see in inspect.  Just to
# see if it feels like an improvement at all.
IRB.conf[:PROMPT_MODE] = :SIMPLE
module Kernel
    def inspect(hits = {})
        return "(#{self.class} ...)" if hits[self]
        hits[self] = true
        if instance_variables.empty?
            "(#{self.class})"
        else
            "(#{self.class} " + 
                instance_variables.map do |x|
                    "#{x}=" + instance_variable_get(x).inspect(hits)
                end.join(' ') +
            ")"
        end
    end
end

class Array
    def inspect(hits = {})
        return "[...]" if hits[self]
        hits[self] = true
        "[" + map { |x| x.inspect(hits) }.join(', ') + "]"
    end
end

class Hash
    def inspect(hits = {})
        return "{...}" if hits[self]
        hits[self] = true
        "{" + map { |k,v| k.inspect(hits) + "=>" + v.inspect(hits) }.join(', ') + "}"
    end
end

class File
    def inspect(hits = nil)
        "(#{self.class} #{path})"
    end
end

class Proc
    def inspect(hits = nil)
        v = "a"
        pvars = []
        (arity < 0 ? -(arity+1) : arity).times do |i|
            pvars << v
            v = v.succ
        end
        pvars << "*#{v}" if arity < 0
        "(Proc |#{pvars.join(',')}|)"
    end
end

class Class
    def make_inspect m = :inspect
        alias_method :the_original_inspect, m
        class_eval %{
            def inspect(hits = nil)
                the_original_inspect
            end
        }
    end
end

class Module; make_inspect :name end
class Regexp; make_inspect end
class String; make_inspect end
class Symbol; make_inspect end
class Time; make_inspect end
class Numeric; make_inspect :to_s end
class Bignum; make_inspect :to_s end
class Fixnum; make_inspect :to_s end
class Float; make_inspect :to_s end
class TrueClass; make_inspect :to_s end
class FalseClass; make_inspect :to_s end
class NilClass; make_inspect end
require 'irb/completion'
