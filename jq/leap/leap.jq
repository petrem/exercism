def is_divisible($x; $y): drem($x; $y) == 0;
def is_leap: is_divisible(.; 4) and (is_divisible(.; 100)|not) or is_divisible(.; 400);
.year | is_leap

                              

                
