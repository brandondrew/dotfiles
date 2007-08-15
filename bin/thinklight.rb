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
  end
end

# test flash
if __FILE__ == $0
  ThinkLight.flash
end
