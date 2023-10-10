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
    Clock.new(hour: @hour + other.hour, minute: @minute + other.minute)
  end

  def -(other)
    Clock.new(hour: @hour - other.hour, minute: @minute - other.minute)
  end

  def ==(other)
    @hour == other.hour and @minute == other.minute
  end
end
