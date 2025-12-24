"""A first, simple version."""

OPERATORS = {
    "plus": lambda lhs, rhs: lhs + rhs,
    "minus": lambda lhs, rhs: lhs - rhs,
    "divided": lambda lhs, rhs: lhs / rhs,
    "multiplied": lambda lhs, rhs: lhs * rhs,
}


def answer(question):
    expression_string = (
        question.removeprefix("What is").strip().removesuffix("?").strip()
    )
    expression = [word for word in expression_string.split(" ") if word != "by"]

    if len(expression) == 0:
        raise ValueError("syntax error")

    while len(expression) > 1:
        print(f"{expression=}")
        try:
            lhs, op, rhs, *rest = expression
        except ValueError as e:
            _lhs, op, *_rest = expression
            try:
                int(op)
            except ValueError:
                if op not in OPERATORS:
                    raise ValueError("unknown operation") from e
            raise ValueError("syntax error") from e

        try:
            lhs = int(lhs)
            rhs = int(rhs)
        except ValueError as e:
            raise ValueError("syntax error") from e

        try:
            op_fn = OPERATORS[op]
        except KeyError as e:
            raise ValueError("syntax error") from e

        expression[:] = [op_fn(lhs, rhs), *rest]
    try:
        return int(expression[0])
    except ValueError as e:
        raise ValueError("syntax error") from e
