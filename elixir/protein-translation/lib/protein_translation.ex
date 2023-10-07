defmodule ProteinTranslation do
    @codon_to_protein %{
      "UGU" => "Cysteine",
      "UGC" => "Cysteine",
      "UUA" => "Leucine",
      "UUG" => "Leucine",
      "AUG" => "Methionine",
      "UUU" => "Phenylalanine",
      "UUC" => "Phenylalanine",
      "UCU" => "Serine",
      "UCC" => "Serine",
      "UCA" => "Serine",
      "UCG" => "Serine",
      "UGG" => "Tryptophan",
      "UAU" => "Tyrosine",
      "UAC" => "Tyrosine",
      "UAA" => "STOP",
      "UAG" => "STOP",
      "UGA" => "STOP"
    }

    defp chunks_of(string, size) do
      # afterwards I found Enum.chunks*, but hey...
      (for start <- 0..(String.length(string))//size, do: String.slice(string, start..(start + size - 1))) |> Enum.take_while(fn x -> x != "" end)
    end

    defp accumulate_codon({:ok, "STOP"}, acc), do: {:halt, acc}
    defp accumulate_codon({:ok, protein}, acc), do: {:cont, [protein|acc]}
    defp accumulate_codon({:error, _}, _), do: {:halt, nil}
    
    @doc """
    Given an RNA string, return a list of proteins specified by codons, in order.
    """
    @spec of_rna(String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
    def of_rna(rna) do
      case rna |> chunks_of(3) |> Enum.map(&of_codon/1) |> Enum.reduce_while([], &accumulate_codon/2) do
        nil -> {:error, "invalid RNA"}
        ps -> {:ok, Enum.reverse(ps)}
      end
    end

    @doc """
    Given a codon, return the corresponding protein

    UGU -> Cysteine
    UGC -> Cysteine
    UUA -> Leucine
    UUG -> Leucine
    AUG -> Methionine
    UUU -> Phenylalanine
    UUC -> Phenylalanine
    UCU -> Serine
    UCC -> Serine
    UCA -> Serine
    UCG -> Serine
    UGG -> Tryptophan
    UAU -> Tyrosine
    UAC -> Tyrosine
    UAA -> STOP
    UAG -> STOP
    UGA -> STOP
    """
    @spec of_codon(String.t()) :: {:ok, String.t()} | {:error, String.t()}
    def of_codon(codon) do
      case @codon_to_protein |> Map.get(codon) do
        nil -> {:error, "invalid codon"}
        x -> {:ok, x}
      end
    end
end
