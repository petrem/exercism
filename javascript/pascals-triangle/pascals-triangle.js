export const rows = (n) => {
  if (n < 1) {
    return [];
  }
  return [...range(n-1)].reduce(
    (acc, _) => acc.concat([next_row(last(acc))]),
    [[1]]
  );
};

const next_row = (row) => zipWith(
  (a, b) => a + b,
  [[0, ...row], [...row, 0]]
);

const zipWith = (op, rows) => rows[0].map(
  (_, idx) => (rows.map((row) => row[idx])).reduce(op)
);

const range = (n) => Array(n).keys(); // stolen from https://stackoverflow.com/a/10050831

const last = (arr) => arr[arr.length - 1];

