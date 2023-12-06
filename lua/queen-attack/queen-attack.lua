local Queen = {}
Queen.__index = Queen

function Queen:new(pos)
   if pos.row < 0 or pos.row > 7 or pos.column < 0 or pos.column > 7 then
      error 'invalid coordinates'
   end
   local newQueen = {row=pos.row, column=pos.column}
   newQueen.can_attack = function (other)
      return newQueen:canAttack(other)
   end
   return setmetatable(newQueen, self)
end

function Queen:__tostring()
   return "Q["..self.row..","..self.column.."]"
end

function Queen:canAttack(other)
   return self.column == other.column
      or self.row == other.row
      or math.abs(self.row - other.row) == math.abs(self.column - other.column)
end


return function (pos) return Queen:new(pos) end
