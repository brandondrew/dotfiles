module Enumerable
  # Thanks Hal Fulton! (Ruby Way, 2nd Ed. pg 289)
  def classify &block
    hash = {}
    self.each do |x|
      result = block.call(x)
      (hash[result] ||= []) << x
    end
    hash
  end

  def ids
    self.map{ |x| x.id }
  end

  def ar_sort
    # if it's an AR object, sort it by id, otherwise by object_id
    # i know, it doesn't work with arrays that have AR and non-AR objects. whatever
    if first.respond_to?('<=>')
      sort
    elsif self.first.is_a?(ActiveRecord::Base)
      sort_by { |record| record['id'] }
    else
      sort_by { |object| object.object_id }
    end
  end

  def detect_value
    each { |o| v = yield(o); return v if v }
    return nil
  end
end
