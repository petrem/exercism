BEGIN {
    FPAT="[[:alpha:]]['[:alpha:]]*"
}
{
    for (k=1; k<=NF; k++) {
        printf("%s", toupper(substr($k,1,1)))
    }
}
