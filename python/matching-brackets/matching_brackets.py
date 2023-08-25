from collections import deque

FLIPPED = {"{": "}", "[": "]", "(": ")"}


def is_paired(input_string):
    stack = deque()
    for c in filter(lambda c: c in "{[()]}", input_string):
        if c in "{[(":
            stack.append(c)
        else:
            if not stack or FLIPPED[stack.pop()] != c:
                return False
    return len(stack) == 0
