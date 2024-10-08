class SpaceAge
  ORBITAL_PERIODS = {
    "earth" => 1,
    "mercury" => 0.2408467,
    "venus" => 0.61519726,
    "mars" => 1.8808158,
    "jupiter" => 11.862615,
    "saturn" => 29.447498,
    "uranus" => 84.016846,
    "neptune" => 164.79132,
  }
  EARTH_YEAR = 365.25 * 24 * 3600
  
  def initialize(seconds)
    @seconds = seconds
  end

  ORBITAL_PERIODS.each do |planet, period|
    define_method("on_#{planet}") do
      @seconds / EARTH_YEAR / period
    end
  end
end
