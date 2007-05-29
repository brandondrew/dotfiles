#!/usr/bin/ruby

require 'rubygems'
require 'yaml'
require 'dbus'
require 'notify'
require 'thinklight' rescue
require 'twitter'
require 'notify'
require 'thinklight'

Bus ||= DBus.session_bus

class Pidgin
  attr_reader :purple
  def initialize
    @service = Bus.service("im.pidgin.purple.PurpleService")
    @purple = @service.object("/im/pidgin/purple/PurpleObject")

    @purple.introspect
    @purple.default_iface ="im.pidgin.purple.PurpleInterface"
  end

  def watch_status
    @twitter = Twitter::Client.new(:login => 'technomancy', :password => File.read(File.expand_path("~/.twitter_password")))
    @purple.on_signal("AccountStatusChanged") do |account, old, new|
      begin
        @twitter.update(current_message) unless current_message == ''
      rescue Twitter::RESTError => re
        puts re
      end
    end
  end

  def notify_messages
    @purple.on_signal("ReceivedImMsg") do |account, user, message|
      Notify.send :message => message, :title => "#{user} says:", :seconds => 7
      begin
        ThinkLight.flash
        ThinkLight.flash
      rescue
      end
    end
  end

  def current_message
    @purple.PurpleSavedstatusGetMessage(@purple.PurpleSavedstatusGetCurrent.first).flatten.first
  end
end

pidgin = Pidgin.new
pidgin.watch_status
pidgin.notify_messages

if __FILE__ == $0
  m = DBus::Main.new
  m << Bus
  m.run
end
#!/usr/bin/ruby

require 'rubygems'
require 'yaml'
require 'dbus'
require 'notify'
require 'thinklight' rescue
require 'twitter'
require 'notify'
require 'thinklight'

Bus ||= DBus.session_bus

class Pidgin
  attr_reader :purple
  def initialize
    @service = Bus.service("im.pidgin.purple.PurpleService")
    @purple = @service.object("/im/pidgin/purple/PurpleObject")

    @purple.introspect
    @purple.default_iface ="im.pidgin.purple.PurpleInterface"
  end

  def watch_status
    @twitter = Twitter::Client.new(:login => 'technomancy', :password => File.read(File.expand_path("~/.twitter_password")))
    @purple.on_signal("AccountStatusChanged") do |account, old, new|
      begin
        @twitter.update(current_message) unless current_message == ''
      rescue Twitter::RESTError => re
        puts re
      end
    end
  end

  def notify_messages
    @purple.on_signal("ReceivedImMsg") do |account, user, message|
      Notify.send :message => message, :title => "#{user} says:", :seconds => 7
      begin
        ThinkLight.flash
        ThinkLight.flash
      rescue
      end
    end
  end

  def current_message
    @purple.PurpleSavedstatusGetMessage(@purple.PurpleSavedstatusGetCurrent.first).flatten.first
  end
end

pidgin = Pidgin.new
pidgin.watch_status
pidgin.notify_messages

if __FILE__ == $0
  m = DBus::Main.new
  m << Bus
  m.run
end
