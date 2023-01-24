BEGIN {
    split("black brown red orange yellow green blue violet grey white", colors)
    split("0     1     2   3      4      5     6    7      8    9", values)
    for (k in colors)
        colorcodes[colors[k]] = values[k]
}
function check(line,    tmp) {
    split(line, tmp)
    for(k in tmp)
        if (!(tmp[k] in colorcodes))
            return 0
    return 1
}
(! check($0)) {
    print "invalid color"
    exit 1
}
{
    print (colorcodes[$1] ? colorcodes[$1] : "") colorcodes[$2]
}
