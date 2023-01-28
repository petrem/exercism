# Task 1: replace `null` with the "name" element of the shopping list.
.name,

# Task 2: replace `null` with the count of the required ingredients.
(.ingredients|length),

# Task 3: replace `null` with the amount of sugar.
(.ingredients[]|select(.item=="sugar").amount.quantity),

# Task 4: replace `null` with the mapping of ingredient names with their substitutions
# (no comma after the last filter)
(
  reduce (
      (.ingredients + ."optional ingredients")[] | select(has("substitute")) | {"\(.item)": .substitute}
    ) as $item ({}; . + $item)
)
