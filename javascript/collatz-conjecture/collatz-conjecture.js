export const steps = (number) => {
    if (number <= 0) {
        throw new Error("Only positive integers are allowed");
    } else if (number === 1) {
        return 0;
    } else {
        return 1 + countIter(takeWhile((x) => x > 1, iterate(hailstone, number)));
    }
}

const hailstone = (n) => n & 1 ? 3 * n + 1 : n / 2;

function* iterate(fn, arg) {
  /* Apply `fn` to arg, then to the result, and so on forever */
  let value = arg;
  while(true) {
    value = fn(value);
    yield value;
  }
}

const countIter = (iter) => iter.reduce((acc) => acc + 1, 0);

function* takeWhile(pred, iter) {
  /* Yield from `iter` while `pred` is true */
  for (const x of iter) {
    if (pred(x)) {
      yield x;
    } else {
      break;
    }
  }
}
