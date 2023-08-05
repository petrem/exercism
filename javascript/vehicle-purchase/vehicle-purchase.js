// @ts-check

/**
 * Determines whether or not you need a licence to operate a certain kind of vehicle.
 *
 * @param {string} kind
 * @returns {boolean} whether a license is required
 */
export function needsLicense(kind) {
  return kind === "truck" || kind === "car";
}

/**
 * Helps choosing between two options by recommending the one that
 * comes first in dictionary order.
 *
 * @param {string} option1
 * @param {string} option2
 * @returns {string} a sentence of advice which option to choose
 */
export function chooseVehicle(option1, option2) {
  return (option1 <= option2 ? option1 : option2) + " is clearly the better choice.";
}

/**
 * Calculates an estimate for the price of a used vehicle in the dealership
 * based on the original price and the age of the vehicle.
 *
 * @param {number} originalPrice
 * @param {number} age
 * @returns {number} expected resell price in the dealership
 */
const AGE_RANGE_DISCOUNT = [
  [0, 0.8],
  [3, 0.7],
  [11, 0.5],
];

const get_discount = (age) => AGE_RANGE_DISCOUNT.filter((x) => x[0] <= age).pop()[1];

export function calculateResellPrice(originalPrice, age) {
  return originalPrice * get_discount(age);
}
