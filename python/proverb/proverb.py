def proverb(*wants, qualifier=None):
    """Return proverb solution."""
    return (
        [
            *[
                f"For want of a {w1} the {w2} was lost."
                for w1, w2 in zip(wants[:-1], wants[1:])
            ],
            " ".join(filter(None, (_CONCLUSION, qualifier, wants[0]))) + ".",
        ]
        if wants
        else []
    )


_CONCLUSION = "And all for the want of a"
