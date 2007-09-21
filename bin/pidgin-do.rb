#!/usr/bin/env ruby
require 'rubygems'
require 'dbus'

# boilerplate for the lose!
@service = DBus.session_bus.service("us.technomancy.pidgin")
obj = @service.object("/us/technomancy/MyInstance")
obj.introspect
obj.default_iface = "us.technomancy.pidgin" 
obj.send ARGV.first
