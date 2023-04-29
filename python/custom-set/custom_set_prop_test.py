from custom_set import CustomSet
from hypothesis import example, given, note
from hypothesis import strategies as st

# Strategies

ELEMENTS = (st.integers(), st.text(), st.dates())
SMALL_INTS = st.integers(min_value=0, max_value=100)


@st.composite
def list_of_things(draw, min_size=0, elements=ELEMENTS):
    return draw(st.lists(st.one_of(elements), min_size=min_size))


def nonempty_list_of_things():
    return list_of_things(min_size=1)


def nonempty_list_of_small_ints():
    return list_of_things(min_size=1, elements=(SMALL_INTS,))


def nonempty_list_of_ints():
    return list_of_things(min_size=1, elements=(st.integers(),))


@st.composite
def disjoint_lists_of_unique_elements(draw, elements=SMALL_INTS, min_size=1):
    xs = draw(st.lists(elements, min_size=min_size, unique=True))
    ys = draw(
        st.lists(elements, min_size=min_size, unique=True).filter(
            lambda ys: set(ys).isdisjoint(xs)
        )
    )
    return xs, ys


# TESTS
# Copied the tests from custom_set_test.py, and adapt them for property testing


def test_sets_with_no_elements_are_empty():
    sut = CustomSet()
    assert sut.isempty() is True


@given(nonempty_list_of_things())
def test_sets_with_elements_are_not_empty(xs):
    sut = CustomSet(xs)
    assert sut.isempty() is False


@given(st.integers(min_value=0, max_value=100))
def test_nothing_is_contained_in_an_empty_set(x):
    # not sure what this is supposed to achieve; the original test
    # checks that `1` is not in the (empty) set.
    sut = CustomSet()
    assert (x in sut) is False


@given(nonempty_list_of_small_ints())
@example([1, 2, 2])  # make sure we have a duplicate
def test_when_the_element_is_in_the_set(xs):
    sut = CustomSet(xs)
    for x in xs:
        assert x in sut


@given(disjoint_lists_of_unique_elements())
def test_when_the_element_is_not_in_the_set(xs_ys):
    xs, ys = xs_ys
    sut = CustomSet(xs)
    for y in ys:
        assert (y not in sut) is True


@given(nonempty_list_of_things())
@example([])
def test_empty_set_is_a_subset_of_another_set(xs):
    # Joined two of the original tests into one (empty set is subset of empty/non-empty)
    set1 = CustomSet()
    set2 = CustomSet(xs)
    assert set1.issubset(set2) is True


@given(nonempty_list_of_things())
def test_non_empty_set_is_not_a_subset_of_empty_set(xs):
    set1 = CustomSet(xs)
    set2 = CustomSet()
    assert set1.issubset(set2) is False


@given(nonempty_list_of_things())
def test_set_is_a_subset_of_set_with_exact_same_elements(xs):
    set1 = CustomSet(xs)
    set2 = CustomSet(xs)
    assert set1.issubset(set2) is True


@given(disjoint_lists_of_unique_elements())
def test_set_is_a_subset_of_larger_set_with_same_elements(xs_ys):
    xs, ys = xs_ys
    set1 = CustomSet(xs)
    set2 = CustomSet(xs + ys)
    assert set1.issubset(set2) is True


@given(disjoint_lists_of_unique_elements(min_size=2))
def test_set_is_not_a_subset_of_set_that_does_not_contain_its_elements(xs_ys):
    xs, ys = xs_ys
    # pick some elements from ys
    ys_left, ys_right = ys[len(ys) // 2 :], ys[: len(ys) // 2]
    set1 = CustomSet(xs + ys_left)
    set2 = CustomSet(xs + ys_right)
    assert set1.issubset(set2) is False


@given(list_of_things())
@example([])
def test_the_empty_set_is_disjoint_with_any_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    assert empty_set.isdisjoint(other_set) is True


@given(list_of_things())
@example([])
def test_any_setis_dijoint_with_the_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    assert other_set.isdisjoint(empty_set) is True


@given(disjoint_lists_of_unique_elements(), st.integers(min_value=0, max_value=100))
def test_sets_are_not_disjoint_if_they_share_an_element(xs_ys, z):
    xs, ys = xs_ys
    set1 = CustomSet(xs + [z])
    set2 = CustomSet(ys + [z])
    assert set1.isdisjoint(set2) is False


@given(disjoint_lists_of_unique_elements(min_size=0))
def test_sets_are_disjoint_if_they_share_no_elements(xs_ys):
    xs, ys = xs_ys
    set1 = CustomSet(xs)
    set2 = CustomSet(ys)
    assert set1.isdisjoint(set2) is True


def test_empty_sets_are_equal():
    # explicitly check empty set equality, because
    # `test_sets_with_elements_are_not_empty` below uses `st.data` which is
    # not compatible with `@example`.
    set1 = CustomSet()
    set2 = CustomSet()
    assert set1 == set2


@given(nonempty_list_of_things())
def test_empty_set_is_not_equal_to_non_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    assert empty_set != other_set


@given(nonempty_list_of_things())
def test_non_empty_set_is_not_equal_to_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    assert other_set != empty_set


@given(list_of_things(), st.data())
def test_sets_with_the_same_elements_are_equal(xs, data):
    set1 = CustomSet(xs)
    set2 = CustomSet(data.draw(st.permutations(xs)))
    assert set1 == set2


@given(disjoint_lists_of_unique_elements())
def test_sets_with_different_elements_are_not_equal(xs_ys):
    xs, ys = xs_ys
    # pick some elements from ys
    ys_left, ys_right = ys[len(ys) // 2 :], ys[: len(ys) // 2]
    set1 = CustomSet(xs + ys_left)
    set2 = CustomSet(xs + ys_right)
    assert set1 != set2


@given(disjoint_lists_of_unique_elements())
def test_set_is_not_equal_to_larger_set_with_same_elements(xs_ys):
    xs, ys = xs_ys
    set1 = CustomSet(xs)
    set2 = CustomSet(xs + ys)
    assert set1 != set2


@given(st.one_of(ELEMENTS))
def test_add_to_empty_set(x):
    sut = CustomSet()
    expected = CustomSet([x])
    sut.add(x)
    assert sut == expected


@given(nonempty_list_of_things(), st.one_of(ELEMENTS))
def test_add_to_non_empty_set(xs, x):
    sut = CustomSet(xs)
    expected = CustomSet(xs + [x])
    sut.add(x)
    assert sut == expected


@given(nonempty_list_of_things(), st.data())
def test_adding_an_existing_element_does_not_change_the_set(xs, data):
    sut = CustomSet(xs)
    x = data.draw(st.sampled_from(xs))
    expected = CustomSet(xs)
    sut.add(x)
    assert sut == expected


@given(list_of_things())
@example([])
def test_intersection_of_an_empty_set_and_another_set_is_an_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    expected = CustomSet()
    assert empty_set.intersection(other_set) == expected


@given(list_of_things())
@example([])
def test_intersection_of_a_set_and_an_empty_set_is_an_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    expected = CustomSet()
    assert other_set.intersection(empty_set) == expected


@given(disjoint_lists_of_unique_elements())
def test_intersection_of_two_sets_with_no_shared_elements_is_an_empty_set(xs_ys):
    xs, ys = xs_ys
    set1 = CustomSet(xs)
    set2 = CustomSet(ys)
    expected = CustomSet()
    assert set1.intersection(set2) == expected


@given(disjoint_lists_of_unique_elements())
def test_intersection_of_two_sets_with_shared_elements_is_a_set_of_the_shared_elements(
    xs_ys,
):
    xs, ys = xs_ys
    # pick some elements from ys
    ys_left, ys_right = ys[len(ys) // 2 :], ys[: len(ys) // 2]
    set1 = CustomSet(xs + ys_left)
    set2 = CustomSet(xs + ys_right)
    expected = CustomSet(xs)
    assert set1.intersection(set2) == expected


@given(list_of_things())
@example([])
def test_difference_of_empty_set_and_another_set_is_an_empty_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    expected = CustomSet()
    assert empty_set - other_set == expected


@given(list_of_things())
@example([])
def test_difference_of_a_set_and_an_empty_set_is_the_set(xs):
    empty_set = CustomSet()
    other_set = CustomSet(xs)
    expected = CustomSet(xs)
    assert other_set - empty_set == expected


@given(disjoint_lists_of_unique_elements(min_size=2))
def test_difference_of_two_non_empty_sets_is_a_set_of_elements_that_are_only_in_the_first_set(
    xs_ys,
):
    xs, ys = xs_ys
    # pick some elements from ys
    ys_left, ys_right = ys[len(ys) // 2 :], ys[: len(ys) // 2]
    set1 = CustomSet(xs + ys_left)
    note(f"Set1: {set1!r}")
    set2 = CustomSet(xs + ys_right)
    note(f"Set2: {set2!r}")
    expected = CustomSet(ys_left)
    note(f"Expected: {expected!r}")
    assert set1 - set2 == expected


@given(nonempty_list_of_things())
def test_union_of_an_empty_set_and_other_is_other(xs):
    set1 = CustomSet()
    set2 = CustomSet(xs)
    expected = CustomSet(xs)
    assert set1 + set2 == expected


@given(nonempty_list_of_things())
def test_union_of_some_set_and_an_empty_set_is_first_set(xs):
    set1 = CustomSet(xs)
    set2 = CustomSet()
    expected = CustomSet(xs)
    assert set1 + set2 == expected


@given(nonempty_list_of_things(), nonempty_list_of_things())
def test_union_is_commutative(xs, ys):
    set1 = CustomSet(xs)
    set2 = CustomSet(ys)
    u1 = set1 + set2
    u2 = set2 + set1
    note(f"Union1: {u1!r} table: {u1._hashtable._table}")
    note(f"Union2: {u2!r} table: {u2._hashtable._table}")
    assert set1 + set2 == set2 + set1


@given(nonempty_list_of_small_ints(), nonempty_list_of_small_ints())
def test_union_of_non_empty_sets_contains_all_unique_elements(xs, ys):
    set1 = CustomSet(xs)
    set2 = CustomSet(ys)
    expected = CustomSet(set(xs + ys))
    assert set1 + set2 == expected
