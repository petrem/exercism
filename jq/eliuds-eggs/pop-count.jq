def countones($n; $c): if $n == 0 then $c else countones($n / 2; $c + ($n % 2)) end;
countones(.number ;0)
