//
// This is only a SKELETON file for the 'ISBN Verifier' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const isValid = (isbn) => {
  const digits = [...isbn].filter((c) => c !== "-").map(digitForLastChar);
  if (digits.length !== 10) {
    return false;
  }
  return isDivisibleByEleven(
    sumArray(tenToOne.map((m, index) => m * digits[index])));
}

const digitForLastChar = (c, index) => c === "X" && index === 9 ? 10 : Number.parseInt(c);
const tenToOne = [...Array(10).keys()].map((x) => x + 1).reverse();  //meh
const sumArray = (arr) => arr.reduce((acc, x) => acc + x, 0);
const isDivisibleByEleven = (n) => n % 11 === 0;

