BEGIN {
    FPAT = "[0-9]"
}
/[^0-9 ]+/ {
    print "false"
    next
}
/^ *[0-9] *$/ {
    print "false"
    next
}
{
    sum = 0
    flipflop = (NF % 2 == 0)
    for (i = 1; i <= NF; i++) {
        if (flipflop)
            sum += luhn_digit($i)
        else
            sum += $i
        flipflop = !flipflop
    }
    if (sum % 10 == 0)
        print "true"
    else
        print "false"
}


function luhn_digit(digit,   doubled)
{
    doubled = digit + digit
    if (doubled > 9) {
        return doubled - 9
    } else {
        return doubled
    }
}
