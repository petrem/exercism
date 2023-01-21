BEGIN {
    FS=""
    find_first = 1
}
find_first {
    split($0, first)
    find_first = 0
    next
}
!find_first {
    if (length(first) != NF) {
        print "strands must be of equal length"
        exit 1
    }
    dist = 0
    for (k = 1; k <= NF ; k++)
        dist += first[k] == $k ? 0 : 1
    print dist
    find_first = 1
}
