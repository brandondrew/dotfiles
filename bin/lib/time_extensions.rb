class Time
  def advance(options)
    d = ::Date.new(year + (options.delete(:years) || 0), month, day)
    d = d >> options.delete(:months) if options[:months]
    d = d +  options.delete(:days)   if options[:days]
    d = d +  options.delete(:weeks) * 7 if options[:weeks]
    change(options.merge(:year => d.year, :month => d.month, :mday => d.day))
  end

  # blah... newer versions of ruby make this private. =P
  def to_date
    Date.civil(self.year, self.mon, self.day)
  end

  alias_method :old_to_s, :to_s
  def to_s(format = nil)
    return old_to_s(format) if format
    self.strftime(TIME_STRING)
  end
end

class Date
  alias_method :old_to_s, :to_s
  
  def to_s(format = nil)
    return old_to_s(format) if format
    self.strftime(DATE_STRING)
  end
end

TimeZoneOptions = TzinfoTimezone.us_zones.map{|z| [z.to_s, z.tzinfo.identifier]}
