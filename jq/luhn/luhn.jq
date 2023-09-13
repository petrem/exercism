def mapped_mul(m): . * m | if . > 9 then . - 9 else . end;

reduce (split("")
         | reverse
         | .[]
         | select(test("\\S"))
         | try tonumber catch (false | halt_error(0))
       ) as $d
       ( [0, 0];
         . as [$a, $c] | [$d | mapped_mul($c % 2 + 1) + $a, $c + 1])
  | .[1] > 1 and .[0] % 10 == 0
