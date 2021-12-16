from itertools import count, takewhile


class TernaryFibTree():

    __slots__ = ("box", "children")

    def __init__(self, box):
        self.box = box
        self.children = (None, None, None)

    def __str__(self):
        c1 = str(self.children[0].box).splitlines()
        c2 = str(self.children[1].box).splitlines()
        c3 = str(self.children[2].box).splitlines()
        return f"""\
{self.box}

{c1[0]} {c2[0]} {c3[0]}
{c1[1]} {c2[1]} {c3[1]}
"""

    def _child1(self):
        return FibBox(None, self.box.q_, self.box.p_, None)

    def _child2(self):
        return FibBox(self.box.q_, self.box.p_, None, None)

    def _child3(self):
        return FibBox(self.box.p_, self.box.q_, None, None)

    def breed(self):
        self.children = (
            TernaryFibTree(self._child1()),
            TernaryFibTree(self._child2()),
            TernaryFibTree(self._child3()),
        )

    def walk_depth_first(self, cutoff):
        """Descends through the tree until the cutoff function, applied
        to the FibBox, returns False.
        """
        if cutoff(self.box):
            yield self.box
            self.breed()
            for i, c in enumerate(self.children):
                for box in c.walk_depth_first(cutoff):
                    yield box
        else:
            self.breed()
            for c in self.children:
                assert not cutoff(c.box), f"{self.box}\n{c.box}"


class FibBox():
    """A matrix representation of a generalized Fibonacci sequence
    (and pythagorean triple). The box looks like:
    [ q q']
    [ p p'] where
    p  = q' + q
    p' = q  + p

    To initialize a box you need to give it at least one of the following:
    q', p'
    q,  q'
    q', p

    It does not check for nonsense input.
    """

    __slots__ = ("q", "q_", "p", "p_")

    def __init__(self, q=None, q_=None, p=None, p_=None):
        self.q = q or ((p - q_) if p else (p_ - q_) // 2)
        self.q_ = q_
        self.p = p or ((q + q_) if q else (q_ + p_) // 2)
        self.p_ = p_ or ((2 * p - q_) if p else (2 * q + q_))

    def pythagorean_triple(self):
        a = 2 * self.q * self.p
        b = self.q_ * self.p_
        c = self.q * self.p_ + self.q_ * self.p
        if a < b:
            return a, b, c
        else:
            return b, a, c

    @property
    def perimeter(self):
        return (self.q + self.p) * (self.q_ + self.p_)

    def __str__(self):
        return f"| {self.q} {self.q_} |\n| {self.p} {self.p_} |"


class RightTriangle():
    __slots__ = ("a", "b", "c")

    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c

    def __str__(self):
        return f"({self.a}, {self.b}, {self.c})"

    def as_tuple(self):
        return self.a, self.b, self.c

    @property
    def perimeter(self):
        return self.a + self.b + self.c

    def multiples(self):
        for i in count(1):
            yield RightTriangle(
                self.a * i,
                self.b * i,
                self.c * i,
            )


def is_triplet(a, b, c):
    return a**2 + b**2 == c ** 2


def triplets_with_sum(n):
    result = set()
    first_box = FibBox(None, 1, None, 3)
    tree = TernaryFibTree(first_box)

    def cutoff(x):
        return x.perimeter <= n

    for primitive_box in tree.walk_depth_first(cutoff):
        primitive_triple = primitive_box.pythagorean_triple()
        primitive_triangle = RightTriangle(*primitive_triple)
        for triangle in takewhile(cutoff, primitive_triangle.multiples()):
            if triangle.perimeter == n:
                result.add(triangle.as_tuple())
    return result
