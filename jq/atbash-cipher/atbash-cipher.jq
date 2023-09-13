def atbash_char: if test("[a-z]")
                 then . as $c | "abcdefghijklmnopqrstuvwxyz"| split("") | .[25 - index($c)]
                 elif test("[0-9]") then .
                 else empty
                 end;

def chunks(size):
  .[:size], if length > size then (.[size:] | chunks(size)) else empty end;

def atbash_chunks(op):
  if op == "encode" then chunks(5)
  elif op == "decode" then .
  else halt_error
  end;

.property as $op
  | [.input.phrase | ascii_downcase | split("") | map(atbash_char) | atbash_chunks($op)]
  | map(join("")) | join(" ")
