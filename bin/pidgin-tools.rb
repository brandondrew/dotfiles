#!/usr/bin/ruby

$LOAD_PATH << File.dirname(__FILE__)

require 'rubygems'
require 'twitter'
require 'hpricot'
require 'yaml'
require 'dbus'
require 'notify'
require 'thinklight'


TwitterClient = Twitter::Client.new(:login => 'technomancy',
                                    :password => File.read(File.expand_path("~/.twitter_password")))

class Pidgin
  attr_reader :purple # for debug

  def initialize
    @service = DBus.session_bus.service("im.pidgin.purple.PurpleService")
    @purple = @service.object("/im/pidgin/purple/PurpleObject")

    @purple.introspect
    @purple.default_iface ="im.pidgin.purple.PurpleInterface"

    init_buddy_icons
  end

  def watch_status
    @purple.on_signal("AccountStatusChanged") do |account, old, new|
      begin
        # statuses that start with space stay on pidgin. also blank ones.
        TwitterClient.update(current_message) unless current_message == '' or current_message.match(/^ /)
      rescue Twitter::RESTError => re
        puts re
      end
    end
  end

  def notify_messages
    @purple.on_signal("ReceivedImMsg") do |account, user, message|
      message.gsub!(/\<[^\>]*\>/, '') # jabber messages have unnecessary XML
      user = user.split(/\//).first # get rid of jabber resource part
      Notify.send(:message => message, :title => "#{user} says:",
                  :seconds => 5,
                  :icon => (File.expand_path("~/.purple/icons/") + '/' +
                            @buddy_icons[user] if @buddy_icons[user]))
      begin
        ThinkLight.flash
        sleep 0.2
        ThinkLight.flash
      rescue
      end
    end
  end

  private
  
  def current_message
    @purple.PurpleSavedstatusGetMessage(@purple.PurpleSavedstatusGetCurrent.first).flatten.first
  end

  def init_buddy_icons
    # would be cool if we could do this thru dbus, but whatever. this is easy.
    @buddy_icons = {}
    @buddy_list = File.read(File.expand_path("~/.purple/blist.xml"))
    @buddy_list.split(/\<buddy/).each do |buddy|
      name = buddy.scan(/\<name\>(.*)\<\/name\>/).flatten.first
      icon = buddy.scan(/\<setting name='buddy_icon' type='string'\>(.*)\<\/setting\>/).flatten.first
      @buddy_icons[name] = icon if icon
    end
  end
end

pidgin = Pidgin.new
pidgin.watch_status
pidgin.notify_messages

if __FILE__ == $0
  m = DBus::Main.new
  m << DBus.session_bus
  m.run
end
