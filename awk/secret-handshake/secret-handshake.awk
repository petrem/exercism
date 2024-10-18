BEGIN { ORS = ""; prev = ""; split("8 4 12 2 10 6 14 1 9 5 13 3 19 7 15", reversed); split("wink,double blink,close your eyes,jump", actions, ","); split("1 2 3 4", redir)}
and($0, 16) { $0 = reversed[and($0, 15)]; split("4 3 2 1", redir) }
and($0, 1) { print actions[redir[1]]; prev = "," }
and($0, 2) { print prev actions[redir[2]]; prev = "," }
and($0, 4) { print prev actions[redir[3]]; prev = "," }
and($0, 8) { print prev actions[redir[4]] }
