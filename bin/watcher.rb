#!/usr/bin/env ruby

require 'rubygems'
require 'open-uri'
require 'digest/sha1'
require 'xmpp4r'
require 'fileutils'

begin
  FileUtils.cd(File.expand_path("~/.watcher"))
  raise unless ARGV.first
rescue
  puts "You need ~/.watcher/config to contain a sender jabber ID, password, and recipient
jabber ID on their own lines. Also pass this script a URL as its first argument."
  exit 1
end

content = open(ARGV.first).read
hash = Digest::SHA1.hexdigest(content)

unless File.exist?(hash)
  include Jabber
  jid, password, to = File.read('config').split("\n")
  message = "#{ARGV.first} changed to #{content[0 .. 200]}"
  
  jid = JID::new("#{jid}/#{`hostname`.chomp}")
  client = Client::new(jid)
  client.connect
  client.auth(password)
  client.send Message.new(to, message).set_type(:normal).set_id('1')

  FileUtils.touch(hash)
end
