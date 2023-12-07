local function splitNumber(s)
   if string.len(s) > 1 then
      return string.sub(s, -1), tonumber(string.sub(s, 1, -2))
   else
      return s, 1
   end
end


return {
   encode = function(s)
      if not s then
         return nil
      end
      local pos = 1
      local chunks = {}
      while pos <= string.len(s) do
         local head = string.sub(s, pos, pos)
         local nextPos, _ = string.find(s, "[^"..head.."]", pos + 1)
         if not nextPos then
            nextPos = #s + 1
         end
         if nextPos - pos == 1 then
            chunks[#chunks+1] = head
         else
            chunks[#chunks+1] = (nextPos - pos)..head
         end
         pos = nextPos
      end
      return table.concat(chunks, '')
   end,

   decode = function(s)
      if not s then
         return nil
      end
      local chunks = {}
      for w in string.gmatch(s, '(%d*[^%d])') do
         chunks[#chunks+1] = string.rep(splitNumber(w))
      end
      return table.concat(chunks, '')
   end
}
