class SimpleCalculator
  ALLOWED_OPERATIONS = ['+', '/', '*'].freeze

  class UnsupportedOperation < ::StandardError
  end

  def self.calculate(first_operand, second_operand, operation)
    begin
      if ALLOWED_OPERATIONS.include?(operation) then
        result = first_operand.public_send(operation, second_operand)
        "#{first_operand} #{operation} #{second_operand} = #{result}"
      else
        raise UnsupportedOperation.new("nil is not a valid operation")
      end
    rescue ZeroDivisionError
      "Division by zero is not allowed."
    rescue TypeError => e
      raise ArgumentError.new(e.message)
    end
  end
end
