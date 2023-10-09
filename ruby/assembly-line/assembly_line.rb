class AssemblyLine
  BASE_CAR_RATE_PER_HOUR = 221
  
  def initialize(speed)
    @speed = speed
    @defect_rate = if speed <= 4 then 1
                   elsif speed <= 8 then 0.9
                   elsif speed == 9 then 0.8
                   else 0.77 end
  end

  def production_rate_per_hour
    BASE_CAR_RATE_PER_HOUR * @speed * @defect_rate
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).floor
  end
end
