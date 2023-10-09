module SavingsAccount
  def self.interest_rate(balance)
    balance < 0 ? 3.213 : balance < 1000 ? 0.5 : balance < 5000 ? 1.621 : 2.475
  end

  def self.annual_balance_update(balance)
    balance * (1 + interest_rate(balance) / 100)
  end

  def self.years_before_desired_balance(current_balance, desired_balance)
    # ok, let's do silly loops
    years = 0
    while current_balance < desired_balance
      current_balance = annual_balance_update(current_balance)
      years += 1
    end
    years
  end
end
