# strictly for the examples in the tests this is good enough
BEGIN {
    FPAT = "([0-9]+)|([A-Za-z])"
    PROCINFO["sorted_in"] = "@ind_str_asc"
}
{
    for (k = 2; k <= NF; k++)
        scores[tolower($k)] = $1
}
END {
    for (letter in scores)
        print letter "," scores[letter]
}
