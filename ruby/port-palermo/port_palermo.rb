module Port
  IDENTIFIER = :PALE

  def self.get_identifier(city)
    city[0, 4].upcase.to_sym
  end

  def self.get_terminal(ship_identifier)
    id_str = ship_identifier.to_s
    (id_str.include?("OIL") or id_str.include?("GAS")) ? :A : :B
  end
end
