BEGIN {
    FS="[ .()+-]*"
    OFS=""
}
/^\s*(\+?1)?(\s|[.-])*\(?([2-9][0-9][0-9])\)?(\s|[.-])*([2-9][0-9][0-9]|\([2-9][0-9][0-9]\))(\s|[.-])*([0-9]{4})\s*$/ {
    $1=$1 # force record to be reconstituted
    FS=""
    $0=$0
    if (NF == 11)
        print substr($0, 2)
    else
        print
    next
}

# Errors
/[[:alpha:]]/ { print "letters not permitted"; exit 1 }
/[^0-9 .()+-]/ { print "punctuations not permitted"; exit 1 }
{
    $1=$1 # force record to be reconstituted
    FS=""
    $0=$0
}
NF < 10 { print "must not be fewer than 10 digits"; exit 1 }
NF == 11 && $1 != 1 { print "11 digits must start with 1"; exit 1 }
NF > 11 { print "must not be greater than 11 digits"; exit 1 }
$(NF - 9) == 0 {print "area code cannot start with zero"; exit 1}
$(NF - 9) == 1 {print "area code cannot start with one"; exit 1}
$(NF - 6) == 0 {print "exchange code cannot start with zero"; exit 1}
$(NF - 6) == 1 {print "exchange code cannot start with one"; exit 1}
{print "unknown error"; exit 1 }
