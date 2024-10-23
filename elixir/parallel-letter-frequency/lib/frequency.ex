defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    worker_pids = Enum.map(1..workers, fn _ ->
      {:ok, pid} = Worker.start_link()
      pid
    end)
    Enum.reduce(Enum.chunk_every(texts, 1), %{}, fn batches, acc ->
      Enum.zip_with(worker_pids, batches, fn pid, batch -> 
        send(pid, {:payload, batch, self()})
      end)
      Enum.reduce(
        for _ <- 1..length(batches) do
          receive do
            partial -> partial
          end
        end,
        %{},
        fn partial, _ -> Map.merge(acc, partial, fn _k, v1, v2 -> v1 + v2 end) end
      )
    end)
  end
end

defmodule Worker do
  def start_link do
    Task.start_link(fn -> loop() end)
  end

  defp loop() do
    receive do
      {:payload, text, caller} ->
        send(caller, count_letters(text))
        loop()
    end
  end

  def count_letters(text) do
    text
    |> String.downcase
    |> String.graphemes
    |> Stream.filter(fn grapheme -> String.match?(grapheme, ~r/^\p{L}$/u) end)
    |> count_things
  end

  def count_things(things) do
    things
    |> Enum.reduce(%{}, fn thing, counts -> Map.update(counts, thing, 1, fn count -> count + 1 end) end)
  end
end
