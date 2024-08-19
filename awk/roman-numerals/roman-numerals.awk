/[^0-9]/ { print "error"; next }
/^[4-9][0-9]{3,}/ { print "error"; next }
function roman(n,    cur) {
    cur = n
    if ((n >= 900 && n < 1000) || (n >=400 && n < 500)) { printf "C" ; return roman(n+100) }
    if ((n >= 90 && n < 100) || (n >=40 && n < 50)) { printf "X" ; return roman(n+10) }
    if (n == 9 || n == 4) { printf "I"; return roman(n+1) }
    if (n >= 1000) {printf "M"; return roman(n-1000)}
    if (n >= 500) {printf "D"; return roman(n-500)}
    if (n >= 100) {printf "C"; return roman(n-100)}
    if (n >= 50) {printf "L"; return roman(n-50)}
    if (n >= 10) {printf "X"; return roman(n-10)}
    if (n >= 5) {printf "V"; return roman(n-5)}
    if (n >= 1) {printf "I"; return roman(n-1)}
}
{ print roman($0) }

