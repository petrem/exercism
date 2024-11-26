def egg_count(v, c=0): return egg_count(v >> 1, c + (v & 1)) if v else c
