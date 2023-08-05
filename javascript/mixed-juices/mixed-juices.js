// @ts-check

/**
 * Determines how long it takes to prepare a certain juice.
 *
 * @param {string} name
 * @returns {number} time in minutes
 */
export function timeToMixJuice(name) {
  switch(name) {
  case 'Pure Strawberry Joy':
    return 0.5;
  case 'Energizer':
  case 'Green Garden':
    return 1.5;
  case 'Tropical Island':
    return 3;
  case 'All or Nothing':
    return 5;
  default:
    return 2.5;
  }
}


const wedgesPerLime = (size) => {
  switch(size) {
  case "small":
    return 6;
  case "medium":
    return 8;
  case "large":
    return 10;
  default:
    throw new Error(`Unknown lime size {size}`);
  }
};

/**
 * Calculates the number of limes that need to be cut
 * to reach a certain supply.
 *
 * @param {number} wedgesNeeded
 * @param {string[]} limes
 * @returns {number} number of limes cut
 */

export function limesToCut(wedgesNeeded, limes) {
  let used = 0;
  while (wedgesNeeded > 0 && limes.length > 0) {
    used++;
    wedgesNeeded -= wedgesPerLime(limes.shift());
  }
  return used;
}

/**
 * Determines which juices still need to be prepared after the end of the shift.
 *
 * @param {number} timeLeft
 * @param {string[]} orders
 * @returns {string[]} remaining orders after the time is up
 */
export function remainingOrders(timeLeft, orders) {
  let juice;
  while (timeLeft > 0 && (juice = orders.shift())) {
    timeLeft -= timeToMixJuice(juice);
    console.log(juice);
    console.log(timeLeft);
  }
  return orders;
}
