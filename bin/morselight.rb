require 'rubygems'
require 'morse'

# if this fails, sudo chmod 777 /proc/acpi/ibm/light

class ThinkLight
  class << self
    def on
      `echo "on" > /proc/acpi/ibm/light`
    end

    def off
      `echo "off" > /proc/acpi/ibm/light`
    end

    def flash(delay=0.2)
      on
      sleep delay
      off
    end

    def dot
      flash 0.2
    end

    def dash
      flash 0.5
    end
  end
end

class MorseLight
  class << self
    def say(message)
      Morse.encode(message).split('').each do |letter|
        sleep 0.1
        case letter
        when '.'
          ThinkLight.dot
        when '-'
          ThinkLight.dash
        when ' '
          sleep 0.2
        end
      end
    end
  end
end
