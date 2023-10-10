class Cipher
  RAND_KEY_LEN = 100
  ALPHABET = [*'a'..'z']
  DELTA = ALPHABET[0].ord

  def to_charcodes(xs)
    (xs.respond_to?("split") ? xs.split("") : xs).map(&:ord)
  end

  def charcodes_to_s(charcodes)
    charcodes.map(&:chr).join
  end

  # Would be nice to be able to use functions, or Proc, but it did't work. I'm missing
  # something.
  def zip_with(f_id, xs, ys)
    xs.zip(ys).map {|(x, y)| self.send(f_id, x, y)}
  end

  def add_in_cipher(x, y)
    DELTA + (x + y - 2 * DELTA).modulo(ALPHABET.size)
  end


  def subtract_in_cipher(x, y)
    DELTA + (x - y).modulo(ALPHABET.size)
  end

  def initialize(key=nil)
    if key.nil? then
      @key = to_charcodes(
        (1..RAND_KEY_LEN).map {|_| ALPHABET[Random.rand(ALPHABET.size)]})
    else
      if not key.match(/^[[:lower:]]+$/) then
        raise ArgumentError.new("we don't like your kind of key around here")
      end
      @key = to_charcodes(key)
    end
  end

  def key
    charcodes_to_s(@key)
  end


  def encode(plain)
    charcodes_to_s(zip_with(:add_in_cipher, to_charcodes(plain), @key.cycle))
  end

  def decode(cipher)
    charcodes_to_s(zip_with(:subtract_in_cipher, to_charcodes(cipher), @key.cycle))
  end
end
