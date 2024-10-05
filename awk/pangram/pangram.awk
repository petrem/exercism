BEGIN { FS=""; split("abcdefghijklmnopqrstuvwxyz", letters) }
{ $0=tolower($0) }
{
    for (k in letters) {
        if (!index($0, letters[k])) {
            print "false"
            next
        }
    }
    print "true"
}

