from functools import partial


class SpaceAge(object):
    EARTH_YEAR = 31557600
    PLANETS = {
        "earth": 1,
        "mercury": 0.2408467,
        "venus": 0.61519726,
        "mars": 1.8808158,
        "jupiter": 11.862615,
        "saturn": 29.447498,
        "uranus": 84.016846,
        "neptune": 164.79132,
    }

    def __init__(self, seconds):
        self.seconds = seconds

    def __getattr__(self, name):
        on, *planet = name.split("_")
        print(f"debug: {on} {planet}")
        if all((on == "on", planet, self.PLANETS.get(planet[0]))):
            return partial(self.age_on_planet, planet[0])
        raise AttributeError(f"{name} not found")

    def age_on_planet(self, planet):
        return round(self.seconds / self.EARTH_YEAR / self.PLANETS[planet], 2)
