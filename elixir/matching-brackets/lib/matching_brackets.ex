defmodule MatchingBrackets do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str), do: to_charlist(str) |> Enum.filter(&(Enum.member?([?{,?},?[,?],?(,?)], &1))) |> is_paired([])

  defp is_paired([], []), do: true
  defp is_paired([], [_|_]), do: false
  defp is_paired([c | rest], stack) when c == ?{ or c == ?[  or c == ?(, do: is_paired(rest, [c|stack])
  defp is_paired([c | rest], [top|stack]) do
    if %{?{ => ?}, ?[ => ?], ?( => ?)}[top] == c do
      is_paired(rest, stack)
    else
      false
    end
  end
  defp is_paired(_, []), do: false
end
