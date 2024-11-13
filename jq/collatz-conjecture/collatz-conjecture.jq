def _steps(n; acc):
  if n == 1 then acc
  elif n % 2 == 0 then _steps(n / 2; acc + 1)
  else _steps(n * 3 + 1; acc + 1) end
;

def steps:
  if . <= 0 then "Only positive integers are allowed" | halt_error(1)
  else _steps(.; 0)
  end
;

