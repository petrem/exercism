CLOSING = {"{": "}", "[": "]", "(": ")"}


def is_paired(input_string):
    stack = []
    for c in filter(lambda c: c in "{[()]}", input_string):
        if c in "{[(":
            stack.append(c)
        elif not stack or CLOSING[stack.pop()] != c:
            return False
    return not stack
