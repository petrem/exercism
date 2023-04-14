"""Solution to Ellen's Alien Game exercise."""


class Alien:
    """Create an Alien object with location x_coordinate and y_coordinate.

    Attributes
    ----------
    (class)total_aliens_created: int
    x_coordinate: int - Position on the x-axis.
    y_coordinate: int - Position on the y-axis.
    health: int - Amount of health points.

    Methods
    -------
    hit(): Decrement Alien health by one point.
    is_alive(): Return a boolean for if Alien is alive (if health is > 0).
    teleport(new_x_coordinate, new_y_coordinate): Move Alien object to new coordinates.
    collision_detection(other): Implementation TBD.
    """

    total_aliens_created = 0

    def __init__(self, x_or_coords, y=None):
        # See below for a so called explanation of why this weird signature.
        if y is None:
            self.x_coordinate, self.y_coordinate = x_or_coords
        else:
            self.x_coordinate = x_or_coords
            self.y_coordinate = y
        self.health = 3
        Alien.total_aliens_created += 1

    def hit(self):
        self.health -= 1

    def is_alive(self):
        return self.health > 0

    def teleport(self, new_x, new_y):
        self.x_coordinate = new_x
        self.y_coordinate = new_y

    def collision_detection(self, other):
        """Will have to implement, eventually."""
        # I really wouldn't advise anyone to put `pass` to mean NotImplemented


def new_aliens_collection(coordinates):
    # Normally, I'd use a list comprehension. But these "learning exercises"
    # are a bit annoying ;-).
    return map(Alien, coordinates)
