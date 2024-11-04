defmodule MatchingBrackets do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str), do: to_charlist(str) |> Enum.filter(&(Enum.member?([?{,?},?[,?],?(,?)], &1))) |> paired?([])

  defp paired?([], []), do: true
  defp paired?([], [_|_]), do: false
  defp paired?([c | rest], stack) when c == ?{ or c == ?[  or c == ?(, do: paired?(rest, [c|stack])
  defp paired?([c | rest], [top|stack]) do
    if %{?{ => ?}, ?[ => ?], ?( => ?)}[top] == c do
      paired?(rest, stack)
    else
      false
    end
  end
  defp paired?(_, []), do: false
end
