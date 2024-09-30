# These variables are initialized on the command line (using '-v'):
# - x
# - y
# - dir

BEGIN {
    x   = x?   x   : 0
    y   = y?   y   : 0
    dir = dir? dir : "north"
    split("north east south west", bearings, /\s+/)
    split("0     1    0     -1  ", deltas_x, /\s+/)
    split("1     0    -1    0   ", deltas_y, /\s+/)
    for (k=1; k <= 4; k++) {
        bearings[bearings[k]] = k
        deltas_x[bearings[k]] = deltas_x[k]
        deltas_y[bearings[k]] = deltas_y[k]
    }
    if (!bearings[dir]) { error = 1; print "invalid direction"; exit }
}
/L/ { dir = bearings[(bearings[dir] + 2) % 4 + 1]  ; next }
/R/ { dir = bearings[(bearings[dir] + 4) % 4 + 1]  ; next }
/A/ { x = x + deltas_x[dir]; y = y + deltas_y[dir] ; next }
    { print "invalid instruction"; error = 1 ; exit }
END {
    if (!error) { print x, y, dir } else { exit -1 }
}
