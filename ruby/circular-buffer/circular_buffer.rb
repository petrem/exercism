class CircularBuffer
  class BufferEmptyException < :: StandardError
  end

  class BufferFullException < :: StandardError
  end

  def initialize(capacity)
    @buffer = [nil] * capacity
    @capacity = capacity
    @size = 0
    @head = 0
  end

  def read
    if @size <= 0 then
      raise BufferEmptyException.new
    end
    tail = (@head + @capacity - @size).modulo(@capacity)
    @size -= 1
    @buffer[tail]
  end

  def write(value)
    if @size >= @capacity then
      raise BufferFullException
    end
    write!(value)
  end

  def write!(value)
    @buffer[@head] = value
    @head = (@head+1).modulo(@capacity)
    @size = [@capacity, @size + 1].min
  end    

  def clear
    @size = 0
  end
end
