module ListOps
  def self.arrays(a)
    c = 0
    a.each {|_| c += 1}
    c
  end

  def self.reverser(a)
    r = []
    a.each_with_index {|e, i| r[a.length - i - 1] = e}
    r
  end

  def self.concatter(a, b)
    r = a.clone
    b.each {|e| r.push e}
    r
  end

  def self.mapper(a)
    r = []
    a.each {|e| r.push(yield e)}
    r
  end

  def self.filterer(a)
    r = []
    a.each {|e| if yield e then r.push(e) end}
    r
  end

  def self.reducer a, z
    r = z
    a.each {|e| r = yield r, e}
    r
  end

  def self.sum_reducer(a)
    self.reducer(a, 0) {|x, y| x + y}
  end

  # there are 2! kinds of people ... :-(
  def self.factorial_reducer(a)
    self.reducer a, 1, &:*
  end
end
