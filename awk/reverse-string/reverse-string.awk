BEGIN {
    FS=OFS=""
    ORS=""
}
{
    for (k=NF; k > 0; --k)
        print $k
    print "\n"
}
