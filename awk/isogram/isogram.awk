BEGIN {
    FS=""
    FPAT = "[[:alpha:]]"
}
{
    delete spotting
    for (k=1; k <= NF; k++)
        if (tolower($k) in spotting) {
            print "false"
            next
        } else {
            spotting[tolower($k)] = 1
        }
    print "true"
}
