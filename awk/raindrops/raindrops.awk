# These variables are initialized on the command line (using '-v'):
# - num

BEGIN {
    if (                                                        \
        out = (num % 3 == 0 ? "Pling":"")                       \
        (num % 5 == 0 ? "Plang":"")                             \
        (num % 7 == 0 ? "Plong":"")                             \
        )
        print out
    else
        print num
}
