def array_add:
  if length == 0 then 0 else first + (.[1:] | array_add) end;

def array_reverse:
  def r(xs): if length == 0 then xs else (first as $head | .[1:] | r([$head] + xs)) end;
  r([]);
  # def r: if length == 0 then empty else (.[1:] | r), first end;
  # [r];

def array_map(f):
  def m: if length == 0 then empty else (first | f), (.[1:] | m) end;
  [m];
