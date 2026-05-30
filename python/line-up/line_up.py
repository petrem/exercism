def line_up(name: str, number: int) -> str:
    """Articulate number"""
    art = (
        ((last2 := number % 100) < 11 or last2 % 100 > 13)
        and {1: "st", 2: "nd", 3: "rd"}.get(number % 10)
    ) or "th"
    return f"{name}, you are the {number}{art} customer we serve today. Thank you!"
