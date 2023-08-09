// @ts-check

export class ArgumentError extends Error {}

export class OverhumidError extends Error {
  constructor(humidity) {
    super(`The temperature is ${humidity} ! Blerh !`);
    this.humidity = humidity;
  }
}
export class OverheatingError extends Error {
  constructor(temperature) {
    super(`The temperature is ${temperature} ! Overheating !`);
    this.temperature = temperature;
  }
}

const EXCESSIVE_HUMIDITY = 70;
const EXCESSIVE_TEMPERATURE = 500;
const CRITICAL_TEMPERATURE = 600;


/**
 * Check if the humidity level is not too high.
 *
 * @param {number} humidityPercentage
 * @throws {Error}
 */
export function checkHumidityLevel(humidityPercentage) {
  if (Number(humidityPercentage) > EXCESSIVE_HUMIDITY) {
    throw new OverhumidError(humidityPercentage);
  }
}

/**
 * Check if the temperature is not too high.
 *
 * @param {number|null} temperature
 * @throws {ArgumentError|OverheatingError}
 */
export function reportOverheating(temperature) {
  if (temperature === null) {
    throw new ArgumentError("Uh oh! Borked sensor!");
  } else if (Number(temperature) > EXCESSIVE_TEMPERATURE) {
    throw new OverheatingError(temperature);
  }
}

/**
 *  Triggers the needed action depending on the result of the machine check.
 *
 * @param {{
 * check: function,
 * alertDeadSensor: function,
 * alertOverheating: function,
 * shutdown: function
 * }} actions
 * @throws {ArgumentError|OverheatingError|Error}
 */
export function monitorTheMachine(actions) {
  try {
    actions.check();
  } catch(error) {
    if (error instanceof ArgumentError) {
      actions.alertDeadSensor();
    } else if (error instanceof OverheatingError) {
      if (Number(error.temperature) > CRITICAL_TEMPERATURE) {
        actions.shutdown();
      } else {
        actions.alertOverheating();
      }
    } else {
      throw error;
    }
  }
}
