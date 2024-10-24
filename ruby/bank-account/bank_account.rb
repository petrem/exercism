=begin
Write your code for the 'Bank Account' exercise in this file. Make the tests in
`bank_account_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/bank-account` directory.
=end
class BankAccount
  def initialize
    @closed = true
    @balance = 0
  end

  def open
    if not @closed then
      raise ArgumentError.new("You can't open an already open account")
    end
    @closed = false
    @balance = 0
  end

  def close
    if @closed then
      raise ArgumentError.new("You can't close an already closed account")
    end
    @closed = true
  end

  def balance
    if @closed then
      raise ArgumentError.new("You can't check the balance of a closed account")
    end
    @balance
  end

  def deposit(amount)
    if @closed then
      raise ArgumentError.new("You can't deposit money into a closed account")
    end
    if amount < 0 then
      raise ArgumentError.new("You can't deposit a negative amount")
    end
    @balance += amount
  end

  def withdraw(amount)
    if @closed then
      raise ArgumentError.new("You can't withdraw money into a closed account")
    end
    if amount < 0 then
      raise ArgumentError.new("You can't withdraw a negative amount")
    end
    if amount > @balance then
      raise ArgumentError.new("You can't withdraw more than you have")
    end
    @balance -= amount
  end
end
