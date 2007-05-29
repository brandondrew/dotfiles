class ThinkLight
  def self.on
    `echo "on" > /proc/acpi/ibm/light`
  end

  def self.off
    `echo "off" > /proc/acpi/ibm/light`
  end

  def self.flash(delay=0.2)
    on
    sleep delay
    off
  end
end

# test flash
if __FILE__ == $0
  ThinkLight.flash
end
