// // eslint-disable-next-line no-extra-boolean-cast
// export const isIsogram = (phrase) => !Boolean(phrase.match(/([a-z]).*\1/i));

export function isIsogram(string) {
  return [...string
    .replaceAll(" ", "")
    .replaceAll("-", "")
    .toLowerCase()]
    .every((x, k, xs) => xs.indexOf(x, k+1) === -1);
}
