//
// This is only a SKELETON file for the 'Raindrops' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const convert = (number) => Object.entries({
    3: "Pling",
    5: "Plang",
    7: "Plong"
  }).reduce((acc, [n, rainWord]) =>
                  acc + (isDivisibleBy(number, n) ? rainWord: ""),
            "") || String(number);

const isDivisibleBy = (n, m) => n % m === 0;
