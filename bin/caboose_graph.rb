#!/usr/bin/env ruby
require 'rubygems'
require 'open-uri'
require 'hpricot'
require 'yaml'

URL, PAGES = ARGV.include?('--local') ? ["users%d.xml", 1] :
  ["http://faces.caboo.se/users.xml?page=%d", 6]

FOUNDERS = ['court3nay', 'mattly', 'corp', 'brynary', 'joshmh']

class String
  def clean
    gsub(/[^a-zA-Z0-9]/, '')
  end

  def highlight
    # bold it if they're a founder... how?
    self # FOUNDERS.include?(self) ? "<b>#{self}</b>" : self
  end
end

invitations = {}
users = {}

1.upto(PAGES) do |page|
  Hpricot(open(URL % page)).search('user').map do |user_xml|
    id = user_xml.search('/id').innerHTML.to_i
    login = user_xml.search('/login').innerHTML
    inviter = user_xml.search('/invited-by').innerHTML.to_i
    
    users[id] = login
    invitations[id] = inviter
  end
end

puts "digraph invitations {
#{invitations.map { |id, inviter| "#{users[id].clean.highlight} -> #{users[inviter].clean.highlight};" if users[inviter] }.join("\n") }
}"

