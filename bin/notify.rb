require 'dbus'

Bus = DBus.session_bus

class Notify
  def self.send(options)
    if options.is_a? String
      message = options
    else
      message = options[:message]
      title = options[:title]
      seconds = options[:seconds]
      icon = options[:icon]
      source = 'notify.rb'
    end

    o = Bus.service("org.freedesktop.Notifications").object("/org/freedesktop/Notifications")
    o.introspect

    i = o["org.freedesktop.Notifications"]

    # TODO: icon?
    i.Notify(source, 0, 'info', title, message, [], {}, seconds * 1000) do |ret, param|
    end
  end
end

# test message
if __FILE__ == $0
  Notify.send "a test message", "test"
end
require 'dbus'

Bus = DBus.session_bus

class Notify
  def self.send(options)
    if options.is_a? String
      message = options
    else
      message = options[:message]
      title = options[:title]
      seconds = options[:seconds]
      icon = options[:icon]
      source = 'notify.rb'
    end

    o = Bus.service("org.freedesktop.Notifications").object("/org/freedesktop/Notifications")
    o.introspect

    i = o["org.freedesktop.Notifications"]

    # TODO: icon?
    i.Notify(source, 0, 'info', title, message, [], {}, seconds * 1000) do |ret, param|
    end
  end
end

# test message
if __FILE__ == $0
  Notify.send "a test message", "test"
end
