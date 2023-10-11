class Bst
  attr_accessor :data
  LEFT = 0
  RIGHT = 1
  
  def initialize(root)
    @data = root
    @subtree = [nil, nil]
  end

  def insert(data)
    branch = data <= @data ? LEFT : RIGHT
    if @subtree[branch].nil? then
      @subtree[branch] = Bst.new(data)
    else
      @subtree[branch].insert(data)
    end
  end

  def left
    @subtree[LEFT]
  end
  
  def right
    @subtree[RIGHT]
  end
  
  def each
    return to_enum(:each) unless block_given?
    @subtree[LEFT].each {|v| yield v} unless @subtree[LEFT].nil?
    yield @data
    @subtree[RIGHT].each {|v| yield v} unless @subtree[RIGHT].nil?
  end
end
