defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) when num > 0 do
    Stream.iterate([1], &next_row/1) |> Enum.take(num)
  end
  
  def rows(_), do: []

  defp next_row(prev) do
    Enum.zip_with(
      Stream.concat(0..0, prev),
      Stream.concat(prev, 0..0),
      fn a, b -> a + b end) |> Enum.to_list()
  end
end
