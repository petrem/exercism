BEGIN { greeted = 0 }
/.+/ { who = $0 }
/^$/ { who = "you" }
     {
         print "One for", who ", one for me."
         greeted = 1
     }
END {
    if (!greeted) print "One for you, one for me."
}
