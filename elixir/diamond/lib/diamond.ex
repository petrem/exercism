defmodule Diamond do
  @doc """
  Given a letter, it prints a diamond starting with 'A',
  with the supplied letter at the widest point.
  """
  @spec build_shape(char) :: String.t()
  def build_shape(letter) do
    Enum.map(0..(letter - ?A), fn current -> build_line(letter, current) end)
    |> reflect
    |> Enum.join
  end

  defp build_line(letter, current) do
    offset = letter - ?A
    pad_left = offset - current
    pad_right = current
    List.duplicate(?\s, pad_left) ++ [?A + current] ++ List.duplicate(?\s, pad_right)
    |> reflect
    |> List.to_string
    |> then(fn line -> line <> "\n" end)
  end
    
  defp reflect([]), do: []
  defp reflect(list) do
    [_|t] = list |> Enum.reverse
    list ++ t
  end
end
