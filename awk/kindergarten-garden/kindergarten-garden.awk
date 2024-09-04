# These variables are initialized on the command line (using '-v'):
# - name

BEGIN {
    students["Alice"] = 1
    students["Bob"] = 2
    students["Charlie"] = 3
    students["David"] = 4
    students["Eve"] = 5
    students["Fred"] = 6
    students["Ginny"] = 7
    students["Harriet"] = 8
    students["Ileana"] = 9
    students["Joseph"] = 10
    students["Kincaid"] = 11
    students["Larry"] = 12

    plants["G"] = "grass"
    plants["C"] = "clover"
    plants["R"] = "radishes"
    plants["V"] = "violets"

    FS = ""
    ORS = " "
}
{
    pos = 2 * students[name] - 1
    printf "%s%s %s",sep,plants[$pos],plants[$(pos+1)]
    sep = " "
}

