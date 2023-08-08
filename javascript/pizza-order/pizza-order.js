/// <reference path="./global.d.ts" />
//
// @ts-check

const PRICES = {
  Margherita: 7,
  Caprese: 9,
  Formaggio: 10,
  ExtraSauce: 1,
  ExtraToppings: 2
};

/**
 * Determine the prize of the pizza given the pizza and optional extras
 *
 * @param {{Pizza|Extra}[]} list of ingredients (first is supposed to be pizza type).
 *
 * @returns {number} the price of the pizza
 */
export function pizzaPrice(...ingredients) {
  if (ingredients.length > 0) {
    const ingredient = ingredients.pop();
    return PRICES[ingredient] + pizzaPrice(...ingredients);
  } else {
    return 0;
  }
}


// trampoline/lazy workaround for lack of TCO taken from
// https://stackoverflow.com/a/37224563/3540204
const trampoline = f => {
  while (typeof f == 'function') f = f();
  return f;
};

const lazy = f => (...args) => () => f(...args);

/**
 * Calculate the prize of the total order, given individual orders
 *
 * @param {PizzaOrder[]} pizzaOrders a list of pizza orders
 * @returns {number} the price of the total order
 */
export function orderPrice(pizzaOrders) {
  const getIngredients = (order) => [order.pizza, ...order.extras];
  const f = lazy(
    (price, orders) => (orders.length <= 0)
      ? price
      : f(price + pizzaPrice(...getIngredients(orders.pop())), orders));
  return trampoline(f(0, pizzaOrders));
}
