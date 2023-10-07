defmodule KitchenCalculator do
  def get_volume({_, value}), do: value
  def to_milliliter({unit, value}) do
    {:milliliter, unit_in_milliliter(unit) * value}
  end
  def from_milliliter({:milliliter, value}, unit) do
    {unit, value / unit_in_milliliter(unit)}
  end
  def convert(volume_pair, unit) do
    from_milliliter(to_milliliter(volume_pair), unit)
  end

  defp unit_in_milliliter(unit) do
    case unit do
      :cup -> 240
      :fluid_ounce -> 30
      :teaspoon -> 5
      :tablespoon -> 15
      :milliliter -> 1
    end
  end
end
