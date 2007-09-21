class Array
  def to_hash
    h = {}
    self.each{ |elt| h[elt[0]] = elt[1]}
    h
  end

  def mean
    self.sum / self.size
  end
  
  def median
    self.sort[self.size / 2]
  end
  
  def variance
    @mean = self.mean
    self.inject(0.0){ |sum, elt| sum + (elt - @mean) ** 2 } / self.size
  end
  
  def std_dev
    Math.sqrt(self.variance)
  end

  def ninety
    self.sort[(self.size * 0.9).to_i]
  end
  
  def invert
    h = {}
    self.each_with_index{ |elt, i| h[i] = elt}
    h
  end
end
