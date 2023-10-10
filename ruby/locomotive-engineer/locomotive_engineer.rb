class LocomotiveEngineer
  def self.generate_list_of_wagons(*wagons)
    wagons
  end

  def self.fix_list_of_wagons(each_wagons_id, missing_wagons)
    w1, w2, l, *ws = each_wagons_id
    [l, *missing_wagons, *ws, w1, w2]
  end

  def self.add_missing_stops
    raise 'Please implement the LocomotiveEngineer.add_missing_stops method'
  end

  def self.extend_route_information(route, more_route_information)
    raise 'Please implement the LocomotiveEngineer.extend_route_information method'
  end
end
