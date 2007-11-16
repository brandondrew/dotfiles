#!/usr/bin/ruby

# by Phil Hagelberg

$LOAD_PATH << File.dirname(__FILE__)

require 'rubygems'
require 'twitter'
require 'hpricot'
require 'yaml'
require 'timeout'
require 'dbus'
require 'notify'

TwitterClient = Twitter::Base.new(*File.read(File.expand_path("~/.twitter_login"))) if File.exist?(File.expand_path("~/.twitter_login"))

class Buddy
  class << self
    attr_accessor :all
    
    def find(name)
      @all.find { |b| b.name == name }
    end
  end
  
  @all = []
  
  def initialize(attribs)
    attribs.each { |k, v| instance_variable_set "@#{k}", v }
    @id = Pidgin.purple.PurpleAccountsFindAny(@account, @protocol).first
    @buddy = Pidgin.purple.PurpleFindBuddy(@id, @name).first
    Buddy.all << self
  end

  attr_reader :name, :protocol, :account, :icon, :id
  
  def icon_path
    @icon and File.expand_path("~/.purple/icons/") + '/' + @icon
  end

  def online?
    Pidgin.purple.PurpleBuddyIsOnline(@buddy).first == 1
  end
end

class Launcher < DBus::Object
  # Dummy class to expose a single method over dbus
  dbus_interface "us.technomancy.pidgin" do
    dbus_method(:launch) { Pidgin.launch }
    dbus_method(:list) { Notify.send :message => Pidgin.online_buddy_names, :title => "Buddies" }
  end
  DBus.session_bus.request_service('us.technomancy.pidgin').export(Launcher.new('/us/technomancy/MyInstance'))
end

class Pidgin
  class << self
    attr_reader :purple

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
                    :icon => Buddy.find(user).icon_path)
      end
    end
    
    def new_conversation(buddy)
      buddy = Buddy.find(buddy) unless buddy.is_a? Buddy
      @purple.PurpleConversationNew(1, buddy.id, buddy.name) if buddy
    end

    def online_buddies
      Buddy.all.select { |b| b.online? }
    end
    
    def online_buddy_names
      online_buddies.map{ |b| b.name }.join("\n")
    end

    def launch
      new_conversation(`echo "#{online_buddy_names}" | dmenu`)
    end
    
    def current_message
      @purple.PurpleSavedstatusGetMessage(@purple.PurpleSavedstatusGetCurrent.first).flatten.first
    end
  end
  
  @service = DBus.session_bus.service("im.pidgin.purple.PurpleService")
  @purple = @service.object("/im/pidgin/purple/PurpleObject")

  @purple.introspect
  @purple.default_iface ="im.pidgin.purple.PurpleInterface"

  @buddy_list = File.read(File.expand_path("~/.purple/blist.xml"))
  @buddy_list.split(/\<buddy/).each do |buddy|
    name = buddy.scan(/\<name\>(.*)\<\/name\>/).flatten.first
    account = buddy.scan(/account='([^']*)'/).flatten.first
    protocol = buddy.scan(/proto='([^']*)'/).flatten.first
    icon = buddy.scan(/\<setting name='buddy_icon' type='string'\>(.*)\<\/setting\>/).flatten.first
    Buddy.new(:name => name, :account => account, :protocol => protocol, :icon => icon) if name and account and protocol
  end
end

Pidgin.watch_status if File.exist? File.expand_path("~/.twitter_login")
Pidgin.notify_messages

if __FILE__ == $0 and !ARGV.include?('--launch')
  begin
    m = DBus::Main.new
    m << DBus.session_bus
    m.run
  rescue
  end while true
else
  Pidgin.launch
end

