#!/usr/bin/env ruby

require 'gconf2'
require 'yaml'

def conf_set(hash, path = '')
  hash.each do |key, value|
    value.is_a?(Hash) ? value.each{ |v| conf_set(value, path + key)} : GConf::Client.default[path + key] = value
  end
end

conf_set YAML.load(File.read("/home/phil/.gconf.yml"))
