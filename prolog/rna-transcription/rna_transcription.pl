rna_transcription(Dna, Rna):-
    string_chars(Dna, DnaChars)
    , dna2rna(DnaChars, RnaChars)
    , string_chars(Rna, RnaChars).

dna2rna([], []).
dna2rna([Dh|Dt], [Rh|Rt]):-
    dna_to_rna(Dh, Rh)
    , dna2rna(Dt, Rt).

dna_to_rna('G','C').
dna_to_rna('C','G').
dna_to_rna('T','A').
dna_to_rna('A','U').
