_V_SET = set("+|")
_H_SET = set("+-")


def rectangles(strings: list[str]) -> int:
    corners = [
        (x, y)
        for x, row in enumerate(strings)
        for y, col in enumerate(row)
        if col == "+"
    ]
    print(corners)

    def _are_connected(tl, br) -> bool:
        # top side
        if set(strings[tl[0]][tl[1] : br[1]]) - _H_SET:
            return False
        # bottom side
        if set(strings[br[0]][tl[1] : br[1]]) - _H_SET:
            return False
        # left side
        if {strings[col][tl[1]] for col in range(tl[0], br[0] + 1)} - _V_SET:
            return False
        # right side
        return not ({strings[col][br[1]] for col in range(tl[0], br[0] + 1)} - _V_SET)

    found = 0
    for pos, top_left in enumerate(corners):
        for bottom_right in corners[pos + 1 :]:
            if (
                top_left[0] != bottom_right[0]
                and top_left[1] < bottom_right[1]
                and _are_connected(top_left, bottom_right)
            ):
                found += 1
    return found
