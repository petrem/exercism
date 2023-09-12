def checks:
  if .sliceLength < 0
  then ("slice length cannot be negative" | halt_error)
  elif .sliceLength == 0
  then ("slice length cannot be zero" | halt_error)
  elif (.series | length) == 0
  then ("series cannot be empty" | halt_error)
  elif .sliceLength > (.series | length)
  then ("slice length cannot be greater than series length" | halt_error)
  else .
  end;

def chunks:
  . as {$series, $sliceLength}
  | range(($series | length) + 1 - $sliceLength)
  | $series[.:($sliceLength + .)];


checks | [chunks]
