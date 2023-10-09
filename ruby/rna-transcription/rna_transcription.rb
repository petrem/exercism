class Complementer
  COMPLEMENTS = {
    'G' => 'C',
    'C' => 'G',
    'T' => 'A',
    'A' => 'U',
  }

  def of_dna(dna)
    # I'm sure this can be done better
    ns = []
    dna.each_char do |nucleotide|
      ns.append(COMPLEMENTS[nucleotide])
    end
    ns.join
  end
end

Complement = Complementer.new
