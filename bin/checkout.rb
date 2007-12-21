#!/usr/bin/env ruby

require 'yaml'

CHECKOUTS = YAML.load(File.read(File.expand_path("~/.checkouts.yml")))

CHECKOUTS.each do |name, opts|
  system "cd ~/src"
  opts[File.exist?(name) ? 'update' : 'fresh'].each { |cmd| puts cmd; system(cmd) }
end
