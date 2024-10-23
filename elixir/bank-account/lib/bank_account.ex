defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank account, making it available for further operations.
  """
  @spec open() :: account
  def open() do
    {:ok, account} = Account.start_link()
    account
  end

  @doc """
  Close the bank account, making it unavailable for further operations.
  """
  @spec close(account) :: any
  def close(account) do
    send(account, {:close})
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer | {:error, :account_closed}
  def balance(account) do
    send(account, {:balance, self()})
    receive do
      {:ok, balance} -> balance
      error -> error
    end
  end

  @doc """
  Add the given amount to the account's balance.
  """
  @spec deposit(account, integer) :: :ok | {:error, :account_closed | :amount_must_be_positive}
  def deposit(account, amount) do
    send(account, {:deposit, amount, self()})
    receive do
      {:ok} -> :ok
      error -> error
    end
  end

  @doc """
  Subtract the given amount from the account's balance.
  """
  @spec withdraw(account, integer) :: :ok | {:error, :account_closed | :amount_must_be_positive | :not_enough_balance}
  def withdraw(account, amount) do
    send(account, {:withdraw, amount, self()})
    receive do
      {:ok} -> :ok
      error -> error
    end
  end
end

defmodule Account do
  def start_link do
    Task.start_link(fn -> loop_open(0) end)
  end

  defp loop_open(balance) do
    receive do
      {:balance, caller} ->
        send(caller, {:ok, balance})
        loop_open(balance)
      {:deposit, amount, caller} ->
        if amount < 0 do
          send(caller, {:error, :amount_must_be_positive})
          loop_open(amount)
        else
          send(caller, {:ok})
          loop_open(balance + amount)
        end
      {:withdraw, amount, caller} ->
        if amount < 0 do
          send(caller, {:error, :amount_must_be_positive})
          loop_open(balance)
        else
          if amount > balance do
            send(caller, {:error, :not_enough_balance})
            loop_open(balance)
          else
            send(caller, {:ok})
            loop_open(balance - amount)
          end
        end
      {:close} ->
        loop_close()
    end
  end

  defp loop_close() do
    receive do
      {_, caller} ->
        send(caller, {:error, :account_closed})
        loop_close()
      {_, _, caller} ->
        send(caller, {:error, :account_closed})
        loop_close()
    end
  end
end
