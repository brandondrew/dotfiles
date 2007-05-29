class Notify
  def self.send(options)
    if options.is_a? String
      title = options
    else
      message = options[:message]
      title = options[:title]
      seconds = options[:seconds]
      icon = options[:icon]
      source = options[:source]
    end

    source ||= 'notify.rb'
    seconds ||= 3

    `notify-send -t #{seconds * 1000} #{'-i "' + icon + '"' if icon} \
\"#{title}\" #{'"' + message + '"' if message}`
  end
end


# test message
if __FILE__ == $0
  Notify.send "a test message"
end
