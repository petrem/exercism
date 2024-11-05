module EliudsEggs
  def self.egg_count(eggs)
    eggs.to_s(2).chars().filter {|c| c == '1'}.count
    # simpler: eggs.to_s(2).count("1")
  end
end
