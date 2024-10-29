module FoodChain
  class Animal
    attr_accessor :name, :utterance
  
    def initialize(name, utterance)
      @name = name
      @utterance = utterance
    end
    
    def simple_stanza
      [
        "I know an old lay who swallowed a #{self.name}.\n",
        self.utterance
      ]
    end
    
    def complex_stanza
      [
        "I know an old lay who swallowed a #{self.name}.\n",
        self.utterance,
      ]
    end

  end
  
  ANIMALS = [
    [Animal.new("fly", "I don't know why she swallowed the fly. Perhaps she'll die.\n"), "simple_stanza"],
    [Animal.new("spider", "It wriggled and jiggled and tickled inside her."), "complex_stanza"],
    [Animal.new("bird", "How absurd to swallow a bird!"), "complex_stanza"],
    [Animal.new("cat", "Imagine that, to swallow a cat!"), "complex_stanza"],
    [Animal.new("dog", "What a hog, to swallow a dog!"), "complex_stanza"],
    [Animal.new("goat", "Just opened her throat and swallowed a goat!"), "complex_stanza"],
    [Animal.new("cow", "I don't know how she swallowed a cow!"), "complex_stanza"],
    [Animal.new("horse", "She's dead, of course!"), "simple_stanza"],
  ]
    


  def self.song
    ANIMALS.map {|(animal, make_stanza)| animal.send make_stanza}.join
  end

end
    
