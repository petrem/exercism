defmodule RobotSimulator do
  @type robot() :: any()
  @type direction() :: :north | :east | :south | :west
  @type position() :: {integer(), integer()}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(direction \\ :north, position \\ {0, 0}) do
    cond do
      not valid_direction?(direction) -> {:error, "invalid direction"}
      not valid_position?(position) -> {:error, "invalid position"}
      true -> {direction, position}
    end
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, instructions) do
    instructions |> String.graphemes |> Enum.reduce_while(robot, &move/2)
    #instructions |> String.graphemes |> hd |> move(robot)
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction({d, _}), do: d

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position({_, p}), do: p

  @directions  [{:north, {0, 1}},
                {:east, {1, 0}},
                {:south, {0, -1}},
                {:west, {-1, 0}} 
               ]

  defp valid_direction?(d), do: @directions |> List.keymember?(d, 0)
  
  defp get_direction(d), do: @directions |> List.keyfind(d, 0)

  defp valid_position?({i1, i2}) when is_integer(i1) and is_integer(i2), do: true
  defp valid_position?(_), do: false

  defp move("R", r), do: {:cont, turn_right(r)}
  defp move("L", r), do: {:cont, turn_left(r)}
  defp move("A", r), do: {:cont, advance(r)}
  defp move(_, _), do: {:halt, {:error, "invalid instruction"}}

  defp turn_right({d, p}), do: {next_direction(d, 1), p}

  defp turn_left({d, p}), do: {next_direction(d, -1), p}

  defp advance({d, p}), do: {d, add_positions(p, get_direction(d) |> elem(1))}
  
  defp add_positions({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}
  
  defp next_direction(d, v) do
    # yes I'm sure this is crappy and there must be better ways
    current_index = Enum.find_index(@directions, fn x -> x == get_direction(d) end)
    Enum.at(@directions, rem(current_index + v, 4)) |> elem(0)
  end
end
