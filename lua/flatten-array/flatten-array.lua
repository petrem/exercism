local function flatten(input)
   local output = {}
   local function go (array)
      for _, value in ipairs(array) do
         if type(value) == 'table' then
            go(value)
         else
            output[#output+1] = value
         end
      end
   end
   go(input)
   return output
end

return flatten
