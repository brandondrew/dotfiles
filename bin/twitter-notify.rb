require 'rubygems'
require 'open-uri'
require 'notify'
require 'twitter'
#require 'hpricot'


# class TwitterFace
#   def self.url(username)
#     response = Hpricot::parse(open("http://twitter.com/#{username}"))
#     element = (response / "h2.thumb img").first
#     raise "Can't find it you on twitter" unless element
#     element.attributes["src"]
#   end
# end


client = Twitter::Client.new(:login => 'technomancy', :password => 'AElenaB')
displayed = []

while true do
  begin
    statuses = client.friend_timeline

    statuses.reverse.each do |stat|
      Notify.send(:message => stat.text, :title => "Twitter: #{stat.user.screen_name}", :seconds => 7) unless displayed.include? stat.id
      displayed << stat.id
    end
  rescue
  end
  sleep 300
end
require 'rubygems'
require 'open-uri'
require 'notify'
require 'twitter'
#require 'hpricot'


# class TwitterFace
#   def self.url(username)
#     response = Hpricot::parse(open("http://twitter.com/#{username}"))
#     element = (response / "h2.thumb img").first
#     raise "Can't find it you on twitter" unless element
#     element.attributes["src"]
#   end
# end


client = Twitter::Client.new(:login => 'technomancy', :password => 'AElenaB')
displayed = []

while true do
  begin
    statuses = client.friend_timeline

    statuses.reverse.each do |stat|
      Notify.send(:message => stat.text, :title => "Twitter: #{stat.user.screen_name}", :seconds => 7) unless displayed.include? stat.id
      displayed << stat.id
    end
  rescue
  end
  sleep 300
end
