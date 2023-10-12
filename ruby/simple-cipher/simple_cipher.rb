class Cipher
  RAND_KEY_LEN = 100
  ALPHABET = ('a'..'z').to_a
  DELTA = ALPHABET[0].ord

  def initialize(key=nil)
    if key.nil?
      @key = to_charcodes(
        (1..RAND_KEY_LEN).map {|_| ALPHABET[Random.rand(ALPHABET.size)]})
    else
      if not key.match(/^[[:lower:]]+$/)
        raise ArgumentError.new("we don't like your kind of key around here")
      end
      @key = to_charcodes_from_s(key)
    end
  end

  def decode(cipher)
    charcodes_to_s zip_with(to_charcodes_from_s(cipher), @key.cycle) { |x, y| subtract_in_cipher(x, y) }
  end

  def encode(plain)
    charcodes_to_s zip_with(to_charcodes_from_s(plain), @key.cycle) { |x, y| add_in_cipher(x, y) }
  end

  def key
    charcodes_to_s(@key)
  end

  private

  def to_charcodes(xs)
    xs.map(&:ord)
  end

  def to_charcodes_from_s(s)
    to_charcodes(s.split "")
  end
  
  def charcodes_to_s(charcodes)
    charcodes.map(&:chr).join
  end

  def zip_with(xs, ys)
    raise ArgumentError, "block must be passed in as an argument" unless block_given?

    xs.zip(ys).map { |(x, y)| yield x, y }
  end

  def add_in_cipher(x, y)
    DELTA + (x + y - 2 * DELTA).modulo(ALPHABET.size)
  end


  def subtract_in_cipher(x, y)
    DELTA + (x - y).modulo(ALPHABET.size)
  end

end
