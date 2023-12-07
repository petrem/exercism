local all_your_base = {}

local function checkBase(base, dir)
   if base < 2 then
      error ('invalid '..dir..' base')
   end
end

local function checkDigits(digits, base)
   for _,digit in ipairs(digits) do
      if digit < 0 then
         error 'negative digits are not allowed'
      elseif  digit >= base then
         error 'digit out of range'
      end
   end
end

all_your_base.convert = function(from_digits, from_base)
   checkBase(from_base, 'input')
   checkDigits(from_digits, from_base)


   local n = 0
   local m = 1
   for j=#from_digits,1,-1 do
      n = n + m * from_digits[j]
      m = m * from_base
   end

   local converter = {}
   function converter.to(to_base)
      checkBase(to_base, 'output')
      if n == 0 then
         return { 0 }
      end
      local to_digits_rev = {}
      local r
      while n > 0 do
         r = n % to_base
         n = math.floor(n / to_base)
         to_digits_rev[#to_digits_rev+1] = r
      end
      local to_digits = {}
      for j=#to_digits_rev,1,-1 do
         to_digits[#to_digits+1] = to_digits_rev[j]
      end
      return to_digits
   end
   return converter
end

return all_your_base
