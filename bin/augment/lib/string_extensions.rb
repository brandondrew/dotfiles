class String
  def colorize(color_code)
    "\e[#{String.lookup_color_code(color_code)}m#{self}\e[0m"
  end

  def colorize_lines(target_lines, color)
    lines = self.split("\n")
    # grrr... why is there no map_with_index!
    target_lines.to_a.each do |affected_line|
      lines[affected_line] = lines[affected_line].colorize(color)
    end
    lines.join("\n")
  end
  
  def self.lookup_color_code(color, foreground = true)
    return color if color.is_a? Fixnum
    {'black' => 30,
      'red' => 31,
      'green' => 32,
      'yellow' => 33,
      'blue' => 34,
      'magenta' => 35,
      'cyan' => 36,
      'white' => 37,
      'default' => 38 }[color.downcase]
  end

  def line(num)
    self.split("\n")[num]
  end
end
