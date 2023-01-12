BEGIN {
    split("a e i o u l n r s t d g b c m p f h v w y k j x q  z", letters, /\s+/)
    split("1 1 1 1 1 1 1 1 1 1 2 2 3 3 3 3 4 4 4 4 4 5 8 8 10 10", scores, /\s+/)
    for (k in letters)
        map[letters[k]] = scores[k]
    FS=OFS=""
}
{
    score = 0
    for (k = 1 ; k <= NF ; k++)
        score += map[tolower($k)]
    print toupper($0), ",", score
    next
}

