defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: count_acc(l, 0)
  
  defp count_acc([], len), do: len
  defp count_acc([_|t], len), do: count_acc(t, len+1)
  
  @spec reverse(list) :: list
  def reverse(l), do: reverse_acc(l, [])
  
  defp reverse_acc([], rev), do: rev
  defp reverse_acc([h|t], rev), do: reverse_acc(t, [h|rev])

  @spec map(list, (any -> any)) :: list
  def map([], _), do: []
  def map([h|t], f), do: [f.(h) | map(t, f)]

  @spec filter(list, (any -> as_boolean(term))) :: list
  # foldr based filter is slower; foldl then reverse is fast; this version is about the same
  def filter([h|t], f), do: if( f.(h), do: [h|filter(t, f)], else: filter(t, f))
  def filter([], _), do: []
  
  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([h|t], acc, f), do: foldl(t, f.(h, acc), f)
  def foldl([], acc, _), do: acc

  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr([h|t], acc, f), do: f.(h, foldr(t, acc, f))
  def foldr([], acc, _), do: acc

  @spec append(list, list) :: list
  def append(a, b), do: foldr(a, b, &([&1|&2]))

  @spec concat([[any]]) :: [any]
  # stolen from https://exercism.org/tracks/elixir/exercises/list-ops/solutions/Pul
  def concat(ll), do: foldl(ll, [], &prepend(&2, &1)) |> reverse()
  
  defp prepend(b, [x | a]), do: prepend([x | b], a)
  defp prepend(b, []), do: b
end
