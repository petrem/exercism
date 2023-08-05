// @ts-check

/**
 * Calculates the total bird count.
 *
 * @param {number[]} birdsPerDay
 * @returns {number} total bird count
 */
export function totalBirdCount(birdsPerDay) {
  let acc = 0;
  for (let j = 0; j < birdsPerDay.length; j++) {
    acc += birdsPerDay[j];
  }
  return acc;
}

/**
 * Calculates the total number of birds seen in a specific week.
 *
 * @param {number[]} birdsPerDay
 * @param {number} week
 * @returns {number} birds counted in the given week
 */
export function birdsInWeek(birdsPerDay, week) {
  let acc = 0;
  const weekStart = (week - 1) * 7;
  for (let j = weekStart ; j < weekStart + 7; j++) {
    acc += birdsPerDay[j];
  }
  return acc;
}

/**
 * Fixes the counting mistake by increasing the bird count
 * by one for every second day.
 *
 * @param {number[]} birdsPerDay
 * @returns {number[]} corrected bird count data
 */
export function fixBirdCountLog(birdsPerDay) {
  for (let j = 0; j < birdsPerDay.length; j += 2) {
    birdsPerDay[j]++;
  }
  return birdsPerDay;

}
