BEGIN {
    FS=","
}
/square_of_sum,[0-9]*/ {
    print square_of_sum($2);
}
/sum_of_squares,[0-9]*/ {
    print sum_of_squares($2);
}
/difference,[0-9]*/ {
    print square_of_sum($2) - sum_of_squares($2)
}

function square_of_sum(n) {
    return (n * (n + 1) / 2)^2
}

function sum_of_squares(n) {
    return n * (n + 1) * (n * 2 + 1) / 6;
}
