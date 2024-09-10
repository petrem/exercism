def primes(limit: int) -> list[int]: from collections import Counter; from itertools import chain; return sorted(k for k,count in Counter(chain.from_iterable(range(k,limit+1,k) for k in range(2,limit+1))).items() if count==1)


# Stolen from https://exercism.org/tracks/raku/exercises/sieve/solutions/habere-et-dispertire
#
# In that Raku solution, they profit from the (surprising for a non-raku programmer like
# me) list-associativity of the ⊝ (symmetric difference) operator. In Raku you can use
# ['infix operator'] ... to create a reduction operator:
#     [+] 1,2,3 is equivalent to ((1+2)+3)
#
# When this is a "normal" left- or right-associative operator, this applies the operator
# from the left or from the right. As it appears, some operators have "list"
# associativity and this produces an entirely different result: the operator is called
# with all the list elements as arguments.
#
# ⊝, or (^) for medieval programmers still not writing code with smileys, is such an
# operator. (This is surely a joke, you say? well, a bit, but see
# https://docs.raku.org/language/optut.) When applied to more than two sets, it does
# not apply to first two sets, then the third, etc. It looks for unique elements
# in ALL the sets. Definitely useful, but not how a set theory discussion of
# symmetrical difference goes:

# > raku
# Welcome to Rakudo™ v2022.12.
# Implementing the Raku® Programming Language v6.d.
# Built on MoarVM version 2022.12.

# To exit type 'exit' or '^D'
# [0] > my $s1 = <1 2 3>; my $s2 = <2 3>; my $s3 = <3>; my $s4 = <4>
# 4
# [1] > (($s1 (^) $s2) (^) $s3) (^) $s4  # left fold
# Set(1 3 4)
# [2] > $s1 (^) ($s2 (^) ($s3 (^) $s4))  # right fold
# Set(1 3 4)
# [3] > $s1 (^) $s2 (^) $s3 (^) $s4  # something that is not a fold
# Set(1 4)
# [4] > [(^)] ($s1, $s2, $s3, $s4)
# Set(1 4)

# The easier on the eye version of the code:

# from collections import Counter
# from itertools import chain


# def primes(limit: int) -> list[int]:
#     return sorted(
#         k for k, count in Counter(
#             chain.from_iterable(range(k, limit + 1, k) for k in range(2, limit + 1))).items()
#         if count == 1)
