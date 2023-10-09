module Port
  IDENTIFIER = :PALE

  def self.get_identifier(city)
    city[0, 4].upcase.to_sym
  end

  def self.get_terminal(ship_identifier)
    %w[OIL GAS].member?(ship_identifier[..2]) ? :A : :B
  end
end
