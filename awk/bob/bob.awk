BEGIN { is_silence = 1 ; is_question = 0; is_shout = 0; RS=""; FS = "" }
/\S/ { is_silence = 0 }
/\?\s*$/ { is_question = 1 }
/^[^a-z]*?[A-Z][^a-z]*$/ { is_shout = 1 }
is_question && is_shout { print "Calm down, I know what I'm doing!"; next }
is_question { print "Sure."; next }
is_shout { print "Whoa, chill out!"; next }
!is_silence { print "Whatever." }
END { if (is_silence) { print "Fine. Be that way!" } }
