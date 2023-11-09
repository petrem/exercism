export const rows = (n) => {
    return (n < 1) ? [] : [...range(n-1)].reduce(
        (acc, _) => acc.concat([next_row(last(acc))]), [[1]]  //eslint-disable-line no-unused-vars
    );
};

const next_row = (row) => zipWith(
  (a, b) => a + b, [[0, ...row], [...row, 0]]
);

const zipWith = (op, rs) => rs[0].map(
  (_, idx) => (rs.map((r) => r[idx])).reduce(op)
);

const range = (n) => Array(n).keys(); // stolen from https://stackoverflow.com/a/10050831

const last = (arr) => arr[arr.length - 1];

