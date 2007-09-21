class Hash
  # returns a hash extracted from the receiver that contains all the values with specified keys
  def extract(*keys)
    keys.flatten.inject({}) {|h,k| h[k] = self[k] if self.has_key?(k); h}
  end
  
  # returns a duplicate hash of the receiver that contains all the values except those of the specified keys
  def omit(*keys)
    keys.flatten.inject(self.dup) {|h,k| h.delete(k); h}
  end

  def delete_keys(*keys)
    if keys.respond_to?(:map)
      keys.map{ |k| self.delete(k) }
    else
      delete(keys)
    end
  end
  
  def self.from_array(ary)
    h = {}
    ary.each{ |elt| h[elt.first] = elt.last }
  end
end
