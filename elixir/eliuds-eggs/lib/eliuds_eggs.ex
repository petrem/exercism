import Bitwise


defmodule EliudsEggs do
  @doc """
  Given the number, count the number of eggs.
  """
  @spec egg_count(number :: integer()) :: non_neg_integer()
  def egg_count(eggs, count \\ 0)
  def egg_count(0, count), do: count
  def egg_count(eggs, count), do: egg_count(eggs >>> 1, count + (eggs &&& 1)) 
end
