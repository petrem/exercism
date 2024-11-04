class DndCharacter
  def self.modifier(score)
    ((score - 10) / 2).floor
  end

  def ability
    ([(rand 1..6), (rand 1..6), (rand 1..6), (rand 1..6)].sort.drop 1).sum
  end

  def initialize
    @strength = ability
    @dexterity = ability
    @constitution = ability
    @intelligence = ability
    @wisdom = ability
    @charisma = ability
  end

  def hitpoints
    10 + DndCharacter.modifier(@constitution)
  end
  
  attr_reader :strength
  attr_reader :dexterity
  attr_reader :constitution
  attr_reader :intelligence
  attr_reader :wisdom
  attr_reader :charisma
end
