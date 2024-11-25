defmodule CircularBuffer do
  @moduledoc """
  An API to a stateful process that fills and empties a circular buffer
  """

  @doc """
  Create a new buffer of a given capacity
  """
  @spec new(capacity :: integer) :: {:ok, pid}
  def new(capacity) do
    start_link(capacity)
  end

  @doc """
  Read the oldest entry in the buffer, fail if it is empty
  """
  @spec read(buffer :: pid) :: {:ok, any} | {:error, atom}
  def read(buffer) do
    send(buffer, {:read, self()})
    receive do
      {:ok, item} -> {:ok, item}
      error -> error
    end
  end

  @doc """
  Write a new item in the buffer, fail if is full
  """
  @spec write(buffer :: pid, item :: any) :: :ok | {:error, atom}
  def write(buffer, item) do
    send(buffer, {:write, item, self()})
    receive do
      :ok -> :ok
      error -> error
    end
  end

  @doc """
  Write an item in the buffer, overwrite the oldest entry if it is full
  """
  @spec overwrite(buffer :: pid, item :: any) :: :ok
  def overwrite(buffer, item) do
    send(buffer, {:overwrite, item})
    :ok
  end

  @doc """
  Clear the buffer
  """
  @spec clear(buffer :: pid) :: :ok
  def clear(buffer) do
    send(buffer, {:clear})
    :ok
  end

  defp start_link(capacity) do
    Task.start_link(fn -> loop({capacity, 0, [], []}) end)
  end

  defp loop({capacity, size, in_buf, out_buf}) do
    receive do
      {:read, caller} ->
        if size == 0 do
          send(caller, {:error, :empty})
          loop({capacity, size, in_buf, out_buf})
        else
          if length(out_buf) == 0 do
            [item | out_buf] = (in_buf |> Enum.reverse)
            send(caller, {:ok, item})
            loop({capacity, size - 1, [], out_buf})
          else
            [item | out_buf] = out_buf
            send(caller, {:ok, item})
            loop({capacity, size - 1, in_buf, out_buf})
          end
        end
      {:write, item, caller} ->
        if size == capacity do
          send(caller, {:error, :full})
          loop({capacity, size, in_buf, out_buf})
        else
          send(caller, :ok)
          loop({capacity, size + 1, [item|in_buf], out_buf})
        end
      {:overwrite, item} ->
        if size == capacity do
          loop({capacity, size, [], out_buf ++ Enum.reverse([item|in_buf]) |> Enum.drop(1)})
        else
          loop({capacity, size + 1 , [item|in_buf], out_buf})
        end
      {:clear} ->
        loop({capacity, 0, [], []})
    end
  end
  
end
