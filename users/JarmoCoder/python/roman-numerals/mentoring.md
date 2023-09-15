I'll try to take into account your stated purpose and refrain from nitpicking on style and some minor things -- however automatisms from code review at work might still kick in, please excuse me if I'm suggesting changes that may seem trivial or having little relevance.

I'm surprised (in a good way) to see a solution using recursion -- this is sadly something that even many developers don't properly grasp -- well done, you. This tells me you are on the right path and committed to go in-depth, therefore I'll try my best to explain my thinking and purpose. I hope I am not being too verbose.

In general, one simplifies code for several reasons: make it more readable, more efficient, more maintainable, etc. And one has several general approaches to do that, for example:
* recognize common patterns that can be abstracted away into functions or other constructs
* remove unused code or redundant code
* use language features or ways to express code that have the same semantic but that are more concise
* change the implementation idea/algorithm (doesn't need to be a fundamental change to achieve some simplification)

The first non-trivial thing you do is to find the smallest diff between the number and the roman digits, and the corresponding roman digit going with that minimum. You do this by smartly leveraging dict data structures. But it can be simplified. A first clue would be that you need to lookup the values of the `differences` dict to find the minimum, both for line 17 and 18, while `min()` would otherwise work on the keys. Could you find a way to let `min()` work on the keys, directly? I promise there's more to this than it might look! :)

The next most conspicuous candidate for simplification is the code that deals with each "length" case (lines 23 to 56). As a first step, I'd look at the cases for length 2 and 3 (and let's worry about the other cases later). Try to extract that code into a separate function, and parametrize it to be able to deal with the two cases. 

In that function, you might also take into account what values could `int_begin` take. Given how it is obtained, could it be 0 or less? Could it be more than 9? Given this, you'll probably find ways to simplify the `if`s conditions and will find code that is never used that you can eliminate.

(At this point you'll probably say "but the code should be defensive and check against incorrect values anyway". I say it depends. If you can logically determine invalid (or unexpected) values can't occur, and the code is not part of your "interface" -- something others would call directly, outside your control --, then such checks are pointless. We can further discuss this and what can be done and when, if you are interested.)

Another thing you could do in this new function is to make the case where `int_begin` is 5..8 more like the other cases: you could actually add the extra `int_begin - 5` unit symbols and substract this from the recursion call.

Some new possibilities for improvement should emerge after this.

Some other things to note, but just as a side dish:
* The last case on line 54 checks for the starting digit to be 1..3. But what if it's not? What should happen, or what do you expect to happen?
* There are some temporary variables at the beginning of the function that are used only once. Most of the times, you can just inline the expression where it is needed (e.g.  on line 15 `int_begin = int(str_number[0])`). There are good reasons to still use such temp variables, e.g. to explain a complex expression, shorten a long line, and obviously, where the value is used several times.

I hope you're still with me at this point, I realize this was long. I also realize these are just some first steps. Hoping you're game to take this approach, once you're done you might see some new possibilities to improve the code, but if you don't, do not worry, I'll give you more hints. 

Do not feel bad for any moment if you find yourself going "ah, I should have seen this" -- it just comes with experience working with something that is fairly abstract and unfamiliar. I prefer giving hints rather than solutions because I expect this way builds such experience, but if you happen to not "get" what I'm saying, the problem is at my end, I didn't explain it right or I assumed the wrong things. Please let me know whenever something is not clear. I'm pretty new at this "mentoring" :)
