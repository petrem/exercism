defmodule Lasagna do
  def expected_minutes_in_oven(), do: 40

  def remaining_minutes_in_oven(spent_minutes) do
    expected_minutes_in_oven() - spent_minutes
  end

  def preparation_time_in_minutes(layers) do
    _preparation_time_per_layer() * layers
  end

  defp _preparation_time_per_layer(), do: 2

  def total_time_in_minutes(layers, spent_minutes) do
    preparation_time_in_minutes(layers) + spent_minutes
  end

  def alarm(), do: "Ding!"

end
