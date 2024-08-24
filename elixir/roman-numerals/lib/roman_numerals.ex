defmodule RomanNumerals do
  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number) when (number < 0 or number > 3999), do: "error"
  def numeral(number) when (number >= 900 and number < 1000) or (number >= 400 and number < 500), do: "C" <> numeral(number + 100)
  def numeral(number) when (number >= 90 and number < 100) or (number >= 40 and number < 50),     do: "X" <> numeral(number + 10)
  def numeral(number) when (number == 9 or number == 4),                                          do: "I" <> numeral(number + 1)
  def numeral(number) when (number >= 1000),                                                      do: "M" <> numeral(number - 1000)
  def numeral(number) when (number >= 500),                                                       do: "D" <> numeral(number - 500)
  def numeral(number) when (number >= 100),                                                       do: "C" <> numeral(number - 100)
  def numeral(number) when (number >= 50),                                                        do: "L" <> numeral(number - 50)
  def numeral(number) when (number >= 10),                                                        do: "X" <> numeral(number - 10)
  def numeral(number) when (number >= 5),                                                         do: "V" <> numeral(number - 5)
  def numeral(number) when (number >= 1),                                                         do: "I" <> numeral(number - 1)
  def numeral(_),                                                                                 do: ""
end
