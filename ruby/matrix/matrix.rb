class Matrix
  def initialize(str)
    @matrix = str.split("\n").map(&:split).map {|row| row.map(&:to_i)}
  end

  def row(j)
    @matrix[j-1]
  end

  def column(j)
    @matrix.transpose[j-1]
  end
end
