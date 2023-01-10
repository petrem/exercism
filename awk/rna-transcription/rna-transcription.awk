BEGIN {
    FS=OFS=""
    split("CGTA", from)
    split("GCAU", to)
    for (k in from)
        map[from[k]] = to[k]
}
/[^CGTA]+/ {
    print "Invalid nucleotide detected."
    exit 1
}
{
    for (k=1; k <= NF; ++k)
        $k = map[$k]
    print
}
