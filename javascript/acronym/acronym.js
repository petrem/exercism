export const parse = (acronym) =>
[...acronym]
    .reduce(([prev, acc], c) =>
        [c, acc + (isInitial(prev, c) ? c : "")],
        [" ", ""])[1]
    .toUpperCase();

const isInitial = (prev, c) =>
      isAlpha(c) && (
          (prev !== "'" && (prev === " " || isPunctuation(prev)))
              || (isLower(prev) && isUpper(c)))

const isPunctuation = (c) => [".", ",", ":", "!", "?", "-"].indexOf(c) > 0;
const isLower = (c) => c.toLowerCase() === c;
const isUpper = (c) => c.toUpperCase() === c;
const isAlpha = (c) => c.toLowerCase() !== c.toUpperCase();
