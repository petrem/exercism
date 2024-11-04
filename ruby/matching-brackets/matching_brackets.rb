module Brackets
  CLOSING = {"(" => ")", "[" => "]", "{" => "}"}
  def self.paired?(string)
    stack = []
    string.chars().filter {|c| "{[()]}".include?(c) }.each do |c|
      if "{[(".include?(c) then 
        stack.push(c)
      elsif stack.empty? or CLOSING[stack.pop()] != c then
        return false
      end
    end
    stack.empty?
  end
end
