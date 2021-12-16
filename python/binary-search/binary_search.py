def find(search_list, value):
    # Not great since deep recursion would be a problem, but can be easily
    # changed to use a loop instead.

    def _find_recursive(left, right):
        if left <= right:
            middle = (left + right) // 2
            if value < search_list[middle]:
                return _find_recursive(left, middle - 1)
            elif value > search_list[middle]:
                return _find_recursive(middle + 1, right)
            else:
                return middle
        else:
            raise ValueError("Value not in list")

    return _find_recursive(0, len(search_list) - 1)
