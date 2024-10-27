class Anagram
  def initialize(word)
    @key = word.downcase
    @normalized_key = normalize(@key)
  end

  def match(words)
    words.select { |word| anagram?(word) }
  end

  def anagram?(word)
    word = word.downcase
    word != @key and normalize(word) == @normalized_key
  end

  def normalize(word)
    word.chars.sort.join
  end
end
