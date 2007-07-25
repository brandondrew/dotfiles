require 'rubygems'
require 'open-uri'
require 'notify'
require 'twitter'
require 'yaml'
#require 'hpricot'


# class TwitterFace
#   def self.url(username)
#     response = Hpricot::parse(open("http://twitter.com/#{username}"))
#     element = (response / "h2.thumb img").first
#     raise "Can't find it you on twitter" unless element
#     element.attributes["src"]
#   end
# end

DOTFILE = File.expand_path('~/.twitter-notify')
client = Twitter::Client.new(:login => 'technomancy', :password => File.read(File.expand_path('~/.twitter_password')))

while true do
  begin
    statuses = client.friend_timeline
    already_displayed = File.read(DOTFILE).split("\n") rescue File.open(DOTFILE, 'w') { |f| f.puts "" }

    statuses.reverse.each do |stat|
      unless already_displayed.include?(stat.id.to_s)
        Notify.send(:message => stat.text, :title => "Twitter: #{stat.user.screen_name}", :seconds => 7) unless stat.text.match(/iphone/i)
        already_displayed << stat.id
        File.open(DOTFILE, 'w') { |f| f.puts already_displayed[-25 .. -1].join("\n")}
        sleep 3
      end
    end
  rescue IOError, SocketError
  end
  sleep 300
end
