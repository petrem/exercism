defmodule RnaTranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

    iex> RnaTranscription.to_rna(~c"ACTG")
    ~c"UGAC"
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    Enum.reverse(Enum.reduce(dna, [], fn c, acc -> [transcribe(c)|acc] end))
  end

  @spec transcribe(char) :: char
  defp transcribe(?C), do: ?G
  defp transcribe(?G), do: ?C
  defp transcribe(?T), do: ?A
  defp transcribe(?A), do: ?U
  
end
