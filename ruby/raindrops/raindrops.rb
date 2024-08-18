module Raindrops
  RAINDROPS = {
    3 => 'Pling',
    5 => 'Plang',
    7 => 'Plong',
  }
  def self.convert(number)
    (rainword = RAINDROPS.map {|divisor, sound| number % divisor == 0 ? sound : ""}.join)
      .empty? ? number.to_s : rainword
  end
end
