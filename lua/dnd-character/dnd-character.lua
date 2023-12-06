math.randomseed(1337)

local Character = {}
Character.__index = Character

local Abilities = {'strength', 'dexterity', 'constitution', 'intelligence', 'wisdom', 'charisma'}


local function ability(scores)
   local sum = 0
   local min = math.huge
   for _,v in ipairs(scores) do
      sum = sum + v
      min = math.min(min, v)
   end
   return sum - min
end

local function roll_dice()
   return {math.random(1, 6)
           ,math.random(1, 6)
           ,math.random(1, 6)
           ,math.random(1, 6)}
end

local function modifier(input)
   return math.floor((input - 10) / 2)
end

function Character:new(name)
   local newDude = {name=name}
   for _,debility in ipairs(Abilities) do
      newDude[debility] = ability(roll_dice())
   end
   newDude.hitpoints = 10 + modifier(newDude.constitution)
   return setmetatable(newDude, self)
end

return {
  Character = Character,
  ability = ability,
  roll_dice = roll_dice,
  modifier = modifier
}
