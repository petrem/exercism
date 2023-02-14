-module(rna_transcription).

-export([to_rna/1]).


to_rna(_Strand) -> [transcribe_nucleotide(X) || X <- _Strand].

transcribe_nucleotide($G) -> $C;
transcribe_nucleotide($C) -> $G;
transcribe_nucleotide($T) -> $A;
transcribe_nucleotide($A) -> $U.