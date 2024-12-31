"""Two-bucket exercise.

If you happen to look at this code for learning purposes, please note that there are
many issues with it: class hierarchy, hashing, whether using __slots__ is warranted...
"""

import typing
from abc import ABC, abstractmethod
from copy import deepcopy
from itertools import chain


BucketId = typing.Literal["one", "two"]


def the_other_bucket_id(bucket: BucketId) -> BucketId:
    return "one" if bucket == "two" else "two"


class Bucket:
    __slots__ = ["bucket_id", "capacity", "volume"]

    def __init__(self, bucket_id, capacity, volume):
        self.bucket_id = bucket_id
        self.capacity = capacity
        self.volume = volume

    def __repr__(self):
        return f"{self.volume}/{self.capacity}"

    def __eq__(self, other):
        return self.bucket_id == other.bucket_id and self.volume == other.volume

    def __hash__(self):
        return hash((self.bucket_id, self.volume))


class State:
    __slots__ = ["buckets"]

    def __init__(self, bucket_1: Bucket, bucket_2: Bucket):
        self.buckets: dict[BucketId, Bucket] = {
            bucket_1.bucket_id: bucket_1,
            bucket_2.bucket_id: bucket_2,
        }

    def __getitem__(self, bucket_id: BucketId) -> Bucket:
        return self.buckets[bucket_id]

    def __repr__(self):
        return f"{self.buckets['one']},{self.buckets['two']}"

    def __eq__(self, other):
        return self.buckets == other.buckets

    def __hash__(self):
        return hash(tuple(self.buckets.values()))

    def goal_bucket(self, goal: int) -> BucketId | None:
        return next(
            (b.bucket_id for b in self.buckets.values() if b.volume == goal), None
        )

    def updated(self, bucket_id: BucketId, volume: int) -> typing.Self:
        buckets = deepcopy(self.buckets)
        buckets[bucket_id].volume = volume
        return self.__class__(buckets["one"], buckets["two"])


class Action(ABC):  # pylint: disable=too-few-public-methods
    @abstractmethod
    def perform(self, state: State) -> State:
        raise NotImplementedError


class SingleBucketAction(Action):  # pylint: disable=too-few-public-methods

    __slots__ = ["bucket_id"]

    def __init__(self, bucket_id: BucketId):
        self.bucket_id = bucket_id


class ActionEmpty(SingleBucketAction):
    def perform(self, state: State) -> State:
        return state.updated(self.bucket_id, 0)

    def __repr__(self):
        return f"Empty({self.bucket_id})"


class ActionFill(SingleBucketAction):
    def perform(self, state: State) -> State:
        return state.updated(self.bucket_id, state[self.bucket_id].capacity)

    def __repr__(self):
        return f"Fill({self.bucket_id})"


class TwoBucketAction(Action):  # pylint: disable=too-few-public-methods

    __slots__ = ["from_bucket", "to_bucket"]

    def __init__(self, bucket_1: BucketId, bucket_2: BucketId):
        self.from_bucket = bucket_1
        self.to_bucket = bucket_2


class ActionPour(TwoBucketAction):

    def perform(self, state: State) -> State:
        amount = min(
            state[self.from_bucket].volume,
            state[self.to_bucket].capacity - state[self.to_bucket].volume,
        )
        return state.updated(
            self.from_bucket, state[self.from_bucket].volume - amount
        ).updated(self.to_bucket, state[self.to_bucket].volume + amount)

    def __repr__(self):
        return f"Pour({self.from_bucket}, {self.to_bucket})"


ACTIONS = [
    ActionEmpty("one"),
    ActionEmpty("two"),
    ActionFill("one"),
    ActionFill("two"),
    ActionPour("one", "two"),
    ActionPour("two", "one"),
]


class Path:

    __slots__ = ["steps", "state"]

    def __init__(self, steps: int, state: State):
        self.steps = steps
        self.state = state

    def __repr__(self):
        return f"Path({self.steps}, {self.state}"

    def extend(self, action: Action) -> typing.Self:
        return self.__class__(self.steps + 1, action.perform(self.state))

    @classmethod
    def paths_from(
        cls, paths: list[typing.Self], explored: set[State]
    ) -> typing.Iterator[typing.Self]:
        frontier = [
            next_path
            for path in paths
            for action in ACTIONS
            if (next_path := path.extend(action)).state not in explored
        ]
        if frontier:
            yield from frontier
            explored = explored.union(path.state for path in frontier)
            yield from cls.paths_from(frontier, explored)


# limit how long paths can get, to avoid exploring ad infinitum
PATHS_LEN_LIMIT = 100


def measure(
    capacity_one: int, capacity_two: int, goal: int, start_bucket: BucketId
) -> tuple[int, BucketId, int]:
    """Return (#actions, goal bucket, content left in other bucket)"""
    if goal < 0 or capacity_one <= 0 or capacity_two <= 0:
        raise ValueError("Huh?")
    if goal > capacity_one and goal > capacity_two:
        raise ValueError("No can do")
    empty_state = State(Bucket("one", capacity_one, 0), Bucket("two", capacity_two, 0))
    empty_path = Path(0, empty_state)
    start_path = empty_path.extend(ActionFill(start_bucket))
    # As per the instructions, "you may not arrive at a state where the initial
    # starting bucket is empty and the other bucket is full". Model this
    # with an "invalid" state that we pretend we've already seen.
    invalid_state = empty_path.extend(
        ActionFill(the_other_bucket_id(start_bucket))
    ).state
    paths_iterator = chain(
        (empty_path, start_path),
        Path.paths_from([start_path], {empty_state, start_path.state, invalid_state}),
    )
    for path in paths_iterator:
        if (goal_bucket := path.state.goal_bucket(goal)) is not None:
            return (
                path.steps,
                goal_bucket,
                path.state.buckets[the_other_bucket_id(goal_bucket)].volume,
            )
        if path.steps > PATHS_LEN_LIMIT:
            raise RuntimeError(
                "Reached paths that are too long without finding a solution."
            )
    raise ValueError("No luck")
