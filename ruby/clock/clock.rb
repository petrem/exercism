class Clock
  attr_accessor :hour, :minute
  
  def initialize(hour: 0, minute: 0)
    carried_hours, @minute = minute.divmod 60
    @hour = (hour + carried_hours).modulo 24
  end

  def to_s
    '%02d:%02d' % [@hour, @minute]
  end

  def +(other)
    carried_hours, minute = (@minute + other.minute).divmod 60
    hour = (@hour + carried_hours + other.hour).modulo 24
    Clock.new(hour: hour, minute: minute)
  end

  def -(other)
    self + Clock.new(hour: -other.hour, minute: -other.minute)
  end

  def ==(other)
    @hour == other.hour and @minute == other.minute
  end
end
