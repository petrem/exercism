from hypothesis import example, given, strategies as st

from custom_set import CustomSet


# Strategies

@st.composite
def disjoint_lists(draw, elements=st.integers(min_value=0, max_value=100)):
    xs = draw(st.lists(elements, min_size=1))
    ys = draw(st.lists(elements, min_size=1).filter(lambda ys: set(ys).isdisjoint(xs)))
    return xs, ys


# TESTS
# Copied the tests from custom_set_test.py, and adapt them for property testing

def test_sets_with_no_elements_are_empty():
    sut = CustomSet()
    assert sut.isempty() is True


@given(st.lists(st.one_of(st.integers(), st.text(), st.dates()), min_size=1))
def test_sets_with_elements_are_not_empty(xs):
    sut = CustomSet(xs)
    assert sut.isempty() is False


@given(st.integers(min_value=0, max_value=200))
def test_nothing_is_contained_in_an_empty_set(x):
    # not sure what this is supposed to achieve; the origin test
    # checks that `1` is not in the (empty) set.
    sut = CustomSet()
    assert (x in sut) is False


@given(st.lists(st.integers(min_value=0, max_value=100), min_size=1))
@example([1, 2, 2])  # make sure we have a duplicate
def test_when_the_element_is_in_the_set(xs):
    sut = CustomSet(xs)
    for x in xs:
        assert x in sut


@given(disjoint_lists())
def test_when_the_element_is_not_in_the_set(xys):
    xs, ys = xys
    sut = CustomSet(xs)
    for y in ys:
        assert (y not in sut) is True

# def test_empty_set_is_a_subset_of_another_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     .assertIs(set1.issubset(set2), True)

# def test_empty_set_is_a_subset_of_non_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([1])
#     .assertIs(set1.issubset(set2), True)

# def test_non_empty_set_is_not_a_subset_of_empty_set():
#     set1 = CustomSet([1])
#     set2 = CustomSet()
#     .assertIs(set1.issubset(set2), False)

# def test_set_is_a_subset_of_set_with_exact_same_elements():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([1, 2, 3])
#     .assertIs(set1.issubset(set2), True)

# def test_set_is_a_subset_of_larger_set_with_same_elements():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([4, 1, 2, 3])
#     .assertIs(set1.issubset(set2), True)

# def test_set_is_not_a_subset_of_set_that_does_not_contain_its_elements():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([4, 1, 3])
#     .assertIs(set1.issubset(set2), False)

# def test_the_empty_set_is_disjoint_with_it():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     .assertIs(set1.isdisjoint(set2), True)

# def test_empty_set_is_disjoint_with_non_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([1])
#     .assertIs(set1.isdisjoint(set2), True)

# def test_non_empty_set_is_disjoint_with_empty_set():
#     set1 = CustomSet([1])
#     set2 = CustomSet()
#     .assertIs(set1.isdisjoint(set2), True)

# def test_sets_are_not_disjoint_if_they_share_an_element():
#     set1 = CustomSet([1, 2])
#     set2 = CustomSet([2, 3])
#     .assertIs(set1.isdisjoint(set2), False)

# def test_sets_are_disjoint_if_they_share_no_elements():
#     set1 = CustomSet([1, 2])
#     set2 = CustomSet([3, 4])
#     .assertIs(set1.isdisjoint(set2), True)

# def test_empty_sets_are_equal():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     .assertEqual(set1, set2)

# def test_empty_set_is_not_equal_to_non_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([1, 2, 3])
#     .assertNotEqual(set1, set2)

# def test_non_empty_set_is_not_equal_to_empty_set():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet()
#     .assertNotEqual(set1, set2)

# def test_sets_with_the_same_elements_are_equal():
#     set1 = CustomSet([1, 2])
#     set2 = CustomSet([2, 1])
#     .assertEqual(set1, set2)

# def test_sets_with_different_elements_are_not_equal():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([1, 2, 4])
#     .assertNotEqual(set1, set2)

# def test_set_is_not_equal_to_larger_set_with_same_elements():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([1, 2, 3, 4])
#     .assertNotEqual(set1, set2)

# def test_add_to_empty_set():
#     sut = CustomSet()
#     expected = CustomSet([3])
#     sut.add(3)
#     .assertEqual(sut, expected)

# def test_add_to_non_empty_set():
#     sut = CustomSet([1, 2, 4])
#     expected = CustomSet([1, 2, 3, 4])
#     sut.add(3)
#     .assertEqual(sut, expected)

# def test_adding_an_existing_element_does_not_change_the_set():
#     sut = CustomSet([1, 2, 3])
#     expected = CustomSet([1, 2, 3])
#     sut.add(3)
#     .assertEqual(sut, expected)

# def test_intersection_of_two_empty_sets_is_an_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     expected = CustomSet()
#     .assertEqual(set1.intersection(set2), expected)

# def test_intersection_of_an_empty_set_and_non_empty_set_is_an_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([3, 2, 5])
#     expected = CustomSet()
#     .assertEqual(set1.intersection(set2), expected)

# def test_intersection_of_a_non_empty_set_and_an_empty_set_is_an_empty_set():
#     set1 = CustomSet([1, 2, 3, 4])
#     set2 = CustomSet()
#     expected = CustomSet()
#     .assertEqual(set1.intersection(set2), expected)

# def test_intersection_of_two_sets_with_no_shared_elements_is_an_empty_set():
#     set1 = CustomSet([1, 2, 3])
#     set2 = CustomSet([4, 5, 6])
#     expected = CustomSet()
#     .assertEqual(set1.intersection(set2), expected)

# def test_intersection_of_two_sets_with_shared_elements_is_a_set_of_the_shared_elements(
#     ,
# ):
#     set1 = CustomSet([1, 2, 3, 4])
#     set2 = CustomSet([3, 2, 5])
#     expected = CustomSet([2, 3])
#     .assertEqual(set1.intersection(set2), expected)

# def test_difference_of_two_empty_sets_is_an_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     expected = CustomSet()
#     .assertEqual(set1 - set2, expected)

# def test_difference_of_empty_set_and_non_empty_set_is_an_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([3, 2, 5])
#     expected = CustomSet()
#     .assertEqual(set1 - set2, expected)

# def test_difference_of_a_non_empty_set_and_an_empty_set_is_the_non_empty_set():
#     set1 = CustomSet([1, 2, 3, 4])
#     set2 = CustomSet()
#     expected = CustomSet([1, 2, 3, 4])
#     .assertEqual(set1 - set2, expected)

# def test_difference_of_two_non_empty_sets_is_a_set_of_elements_that_are_only_in_the_first_set(
#     ,
# ):
#     set1 = CustomSet([3, 2, 1])
#     set2 = CustomSet([2, 4])
#     expected = CustomSet([1, 3])
#     .assertEqual(set1 - set2, expected)

# def test_union_of_empty_sets_is_an_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet()
#     expected = CustomSet()
#     .assertEqual(set1 + set2, expected)

# def test_union_of_an_empty_set_and_non_empty_set_is_the_non_empty_set():
#     set1 = CustomSet()
#     set2 = CustomSet([2])
#     expected = CustomSet([2])
#     .assertEqual(set1 + set2, expected)

# def test_union_of_a_non_empty_set_and_empty_set_is_the_non_empty_set():
#     set1 = CustomSet([1, 3])
#     set2 = CustomSet()
#     expected = CustomSet([1, 3])
#     .assertEqual(set1 + set2, expected)

# def test_union_of_non_empty_sets_contains_all_unique_elements():
#     set1 = CustomSet([1, 3])
#     set2 = CustomSet([2, 3])
#     expected = CustomSet([3, 2, 1])
#     .assertEqual(set1 + set2, expected)
