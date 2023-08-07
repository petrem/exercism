// @ts-check

/**
 * Double every card in the deck.
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with every card doubled
 */
export function seeingDouble(deck) {
  return deck.map(x => 2 * x);
}

/**
 *  Creates triplicates of every 3 found in the deck.
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with triplicate 3s
 */
export function threeOfEachThree(deck) {
  // Mutating the accumulator in place would be, to my limited understanding of js,
  // more efficent (and it still keeps the result "pure").
  return deck.reduce(
    (acc, card) => (card === 3
                    ? acc.splice(acc.length, 0, card, card, card)
                    : acc.push(card), acc)
    , []);
}

/**
 * Extracts the middle two cards from a deck.
 * Assumes a deck is always 10 cards.
 *
 * @param {number[]} deck of 10 cards
 *
 * @returns {number[]} deck with only two middle cards
 */
export function middleTwo(deck) {
  return deck.slice(4, 6);
}

/**
 * Moves the outside two cards to the middle.
 *
 * @param {number[]} deck with even number of cards
 *
 * @returns {number[]} transformed deck
 */

export function sandwichTrick(deck) {
  if (deck.length < 2) {
    return [...deck];
  }
  const halfIndex = Math.trunc(deck.length / 2);
  return [
    ...deck.slice(1, halfIndex)
    , deck.at(-1)
    , deck.at(0)
    , ...deck.slice(halfIndex, deck.length - 1)];
}

/**
 * Removes every card from the deck except 2s.
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with only 2s
 */
export function twoIsSpecial(deck) {
  return deck.filter(card => card === 2);
}

const numericCmp = (a, b) => a - b;

/**
 * Returns a perfectly order deck from lowest to highest.
 *
 * @param {number[]} deck shuffled deck
 *
 * @returns {number[]} ordered deck
 */
export function perfectlyOrdered(deck) {
  return deck.sort(numericCmp);
}

/**
 * Reorders the deck so that the top card ends up at the bottom.
 *
 * @param {number[]} deck
 *
 * @returns {number[]} reordered deck
 */
export function reorder(deck) {
  return deck.reverse(numericCmp);
}
