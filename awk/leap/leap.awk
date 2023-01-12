/[^0-9]/ { print "error"; next }
function is_divisible(n, k) {
    return n % k == 0
}
is_divisible($0, 4) && !is_divisible($0, 100) || is_divisible($0, 400) { print "true" ; next }
{ print "false" }
