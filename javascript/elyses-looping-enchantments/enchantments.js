// @ts-check


const isEven = x => x % 2 === 0;
const isOdd = x => x % 2 !== 0;
const ODDITY = {
  true: isEven,
  false: isOdd
};

  // No better exercise for forEach? Ugh...
const countByForEach = (xs, predicate) => {
  let count = 0;
  xs.forEach(x => {
    if (predicate(x)) {
      count ++;
    }
  });
  return count;
};

  // No better exercise for for..of..? Ugh...
const countByForOf = (xs, predicate) => {
  let count = 0;
  for(const x of xs) {
    if (predicate(x)) {
      count ++;
    }
  }
  return count;
};

/**
 * Determine how many cards of a certain type there are in the deck
 *
 * @param {number[]} stack
 * @param {number} card
 *
 * @returns {number} number of cards of a single type there are in the deck
 */
export function cardTypeCheck(stack, card) {
  return countByForEach(stack, x => x === card);
}

/**
 * Determine how many cards are odd or even
 *
 * @param {number[]} stack
 * @param {boolean} type the type of value to check for - odd or even
 * @returns {number} number of cards that are either odd or even (depending on `type`)
 */

export function determineOddEvenCards(stack, type) {
  return countByForOf(stack, ODDITY[type]);
}
