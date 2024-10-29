# fairly shitty code, sorry; lots to improve

module FoodChain
  REVNAMES = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"].reverse
  class Animal
    attr_accessor :name, :utterance

    def initialize(name, utterance)
      @name = name
      @utterance = utterance
    end

    def simple_stanza
      [
        "I know an old lady who swallowed a #{self.name}.\n",
        self.utterance,
        "\n",
      ]
    end

    def complex_stanza
      idx = REVNAMES.find_index(self.name)
      [
        "I know an old lady who swallowed a #{self.name}.\n",
        self.utterance,
        zip_with(REVNAMES[idx..], REVNAMES[(idx+1)..]) {
          |x,y| "She swallowed the #{x} to catch the #{hack(y)}.\n"
        },
        ANIMALS[0][0].utterance,
        "\n",
      ].flatten
    end

    def zip_with(xs, ys)
      raise ArgumentError, "block must be passed in as an argument" unless block_given?
      xs.zip(ys).filter {|(x, y)| x and y}.map { |(x, y)| yield x, y }
    end

    def hack(animal)
      animal == "spider" ? "spider that wriggled and jiggled and tickled inside her" : animal
    end
  end

  ANIMALS = [
    [Animal.new("fly", "I don't know why she swallowed the fly. Perhaps she'll die.\n"), "simple_stanza"],
    [Animal.new("spider", "It wriggled and jiggled and tickled inside her.\n"), "complex_stanza"],
    [Animal.new("bird", "How absurd to swallow a bird!\n"), "complex_stanza"],
    [Animal.new("cat", "Imagine that, to swallow a cat!\n"), "complex_stanza"],
    [Animal.new("dog", "What a hog, to swallow a dog!\n"), "complex_stanza"],
    [Animal.new("goat", "Just opened her throat and swallowed a goat!\n"), "complex_stanza"],
    [Animal.new("cow", "I don't know how she swallowed a cow!\n"), "complex_stanza"],
    [Animal.new("horse", "She's dead, of course!"), "simple_stanza"],
  ]

  def self.song
    ANIMALS.map {|(animal, make_stanza)| animal.send make_stanza}.join
  end

end
