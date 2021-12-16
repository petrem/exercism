export const age = (planet, seconds) => {
    let prettyPreciseAge = seconds / EARTH_YEAR / PLANETS[planet];
    return Math.round(prettyPreciseAge * 100) / 100;
};

const EARTH_YEAR = 31557600;
const PLANETS = {
    "earth": 1,
    "mercury": 0.2408467,
    "venus": 0.61519726,
    "mars": 1.8808158,
    "jupiter": 11.862615,
    "saturn": 29.447498,
    "uranus": 84.016846,
    "neptune": 164.79132,
};
