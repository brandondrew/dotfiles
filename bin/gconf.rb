# -*- coding: utf-8 -*-
require 'yaml'

# gconf settings

class Hash
  def gconf(path = '')
    each { |key, value| value.gconf(path + key) }
  end
end

class Object
  def gconf(path, type)
    system("gconftool-2 --set \"#{path}\" --type #{type} \"#{self}\"")
  end
end

class String
  def gconf(path)
    super(path, 'string')
  end
end

class Fixnum
  def gconf(path)
    super(path, 'int')
  end
end

# Seriously, Ruby? No common superclass between true and false? What's wrong with you‽
class TrueClass
  def gconf(path)
    super(path, 'bool')
  end
end

class FalseClass
  def gconf(path)
    super(path, 'bool')
  end
end

YAML.load(File.read(File.expand_path("~/.gconf.yml"))).gconf if __FILE__ == $0
