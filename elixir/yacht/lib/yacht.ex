defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(:ones, dice), do: dice |> score_n(1)
  def score(:twos, dice), do: dice |> score_n(2)
  def score(:threes, dice), do: dice |> score_n(3)
  def score(:fours, dice), do: dice |> score_n(4)
  def score(:fives, dice), do: dice |> score_n(5)
  def score(:sixes, dice), do: dice |> score_n(6)

  def score(:full_house, dice) do
    case faces_by_counts(dice) do
      [{_, 2}, {_, 3}] -> Enum.sum(dice)
      _ -> 0
    end
  end
  
  def score(:four_of_a_kind, dice) do
    {which, how_many} = top_face_and_count(dice)
    if how_many >= 4 do 4 * which else 0 end
  end
  
  def score(:little_straight, dice), do: straight(dice, 1)
  def score(:big_straight, dice), do: straight(dice, 2)
  
  def score(:choice, dice), do: Enum.sum(dice)

  def score(:yacht, dice) do
    if (dice |> MapSet.new |> MapSet.size) == 1 do 50 else 0 end
  end

  defp score_n(dice, n), do: dice |> Enum.count(fn x -> x == n end) |> Kernel.*(n)

  defp top_face_and_count(dice) do
    dice |> Enum.frequencies |> Map.to_list |> Enum.max_by(&elem(&1, 1))
  end

  defp faces_by_counts(dice) do
    dice |> Enum.frequencies |> Map.to_list |> Enum.sort_by(&elem(&1, 1))
  end

  defp straight(dice, start) do
    if (dice |> Enum.sort) == Enum.to_list(start..(start+4)) do 30 else 0 end
  end
end
