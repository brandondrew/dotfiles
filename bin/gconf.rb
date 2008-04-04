#!/usr/bin/env ruby

require 'gconf2'
require 'yaml'

# TODO: rewrite this as a lambda using the Y-combinator. =)
def conf_set(hash, path = '')
  hash.each do |key, value|
    if value.is_a?(Hash)
      value.each{ |v| conf_set(value, path + key)}
    else
      GConf::Client.default[path + key] = value
    end
  end
end

conf_set YAML.load(File.read("/home/phil/.gconf.yml"))
