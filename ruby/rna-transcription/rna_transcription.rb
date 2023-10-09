class Complement
  COMPLEMENTS = {
    'G' => 'C',
    'C' => 'G',
    'T' => 'A',
    'A' => 'U',
  }

  def self.of_dna(dna)
    # I'm sure this can be done better (i.e. less imperatively)
    ns = []
    dna.each_char do |nucleotide|
      ns.append(COMPLEMENTS[nucleotide])
    end
    ns.join
  end
end

