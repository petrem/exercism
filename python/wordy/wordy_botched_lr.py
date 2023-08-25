"""YetAnotherOverEngineeredSolution for Wordy.

Resembles a LL parser.
"""
import re
from abc import ABCMeta, abstractmethod
from collections import deque
from dataclasses import dataclass, field
from enum import Enum, auto
from itertools import chain, repeat, starmap, zip_longest
from operator import methodcaller
from typing import Any, Iterable, Optional, Sequence, List, Callable


def answer(question):
    #    print(list(tokenize(question, LEX_TOKENS, BLANKS)))
    expr = parse(question, RULES)
    return expr.value


class Token(Enum):
    QUERY = auto()
    NUMBER = auto()
    QUESTION_MARK = auto()
    PLUS = auto()


# @global_enum, where are you?
QUERY, NUMBER, QUESTION_MARK, PLUS = Token.__members__.values()


BLANKS = " \t\n"
LEX_TOKENS = {
    QUERY: r"What is",
    NUMBER: r"-?\d+",
    QUESTION_MARK: r"\?",
    PLUS: r"plus",
}


def tokenize(text: str, tokens: dict, blanks: str) -> Iterable[tuple[Token, str]]:
    """Tokenize (lex) ``text`` using ``tokens`` and ignoring ``blanks``."""
    text = text.lstrip(blanks)
    while text:
        for token, token_re in tokens.items():
            if (m := re.match(token_re, text)):
                yield token, m.group(0)
                text = text[m.end(0):].lstrip(blanks)
                break
        else:
            raise ValueError("unknown operation")


class Symbol(metaclass=ABCMeta):
    """Abstract superclass for terminal or non-terminal symbols."""

    @abstractmethod
    def matches(self, other):
        raise NotImplementedError

    @abstractmethod
    def __repr__(self):
        raise NotImplementedError


class Terminal(Symbol):
    """A terminal element."""

    def __init__(self, token: Token, lexval: Optional[str] = None):
        super().__init__()
        self.token = token
        self.lexval = lexval

    def matches(self, other):
        if (
            isinstance(other, self.__class__)
            and self.token == other.token
        ):
            return True
        return False

    @property
    def value(self):
        return self.lexval

    def __repr__(self):
        return f"<Terminal: {self.token.name} '{self.lexval}'>"


class NonTerminal(Symbol):
    """A non-terminal element."""

    def __init__(self, name: str, value: Any = None):
        super().__init__()
        self.name = name
        self.value = value

    def matches(self, other):
        if (
            isinstance(other, self.__class__)
            and self.name == other.name
        ):
            return True
        return False

    def __repr__(self):
        return f"<NonTerminal: {self.name} {self.value}>"


@dataclass
class Derivation:
    """Grammar derivation rule."""
    derived_symbol: str
    derivation: Sequence[Symbol]
    action: Callable
    valency: int = field(init=False)

    def __post_init__(self):
        self.valency = len(self.derivation)


def zip_left(iter1, iter2, default=None):
    """Zip ``iter1`` and ``iter2`` for as long as the first iterable.

    If second iterable is exhausted, fill in with a ``default`` (or None).
    """
    return zip(iter1, chain(iter2, repeat(default)))


def identity(x):
    """Identity function."""
    return x


class ParserStack:
    def __init__(self):
        self.queue = deque()

    def shift(self, elem: Symbol):
        print(f"DEBUG: shifting {elem}")
        self.queue.appendleft(elem)

    def reduce(self, rule: Derivation):
        print(
            f"DEBUG:\trule for {rule.derived_symbol}^{rule.valency} ",
            end="",
        )
        if all(a.matches(b) for a, b in zip_left(rule.derivation, self.queue)):
            values = [self.queue.popleft().value for _ in range(rule.valency)]
            print(
                f"matched with {values=}"
            )
            value = rule.action(
                *reversed(values)
            )
            elem = NonTerminal(rule.derived_symbol, value)
            self.shift(elem)
            return True
        print("not matched")
        return False

    def result(self):
        if len(self.queue) == 1:
            return self.queue.popleft()
        raise ValueError("syntax error")

    def __repr__(self):
        return f"<ParserStack: {self.queue}>"


RULES = [
    # <factor> ::= NUMBER
    Derivation(
        "factor",
        [Terminal(NUMBER)],
        lambda v: int(v),
    ),
    # <term> ::= <factor>
    Derivation("term", [NonTerminal("factor")], identity),
    # <expr> ::= <term>
    Derivation("expr", [NonTerminal("term")], identity),
    # <expr> ::= <expr> "plus" <term>
    Derivation("expr", [NonTerminal("expr"), Terminal(PLUS), NonTerminal("term")], identity),
    # <question> ::= "what is" <expr> "?"
    Derivation(
        "question",
        [Terminal(QUERY), NonTerminal("expr"), Terminal(QUESTION_MARK)],
        identity,
    ),
]


def parse(expression: str, grammar: List[Derivation]):
    stack = ParserStack()
    tokens = tokenize(expression, LEX_TOKENS, BLANKS)
    while True:
        print(f"DEBUG: {stack=}")
        for rule in grammar:
            if stack.reduce(rule):
                break
        else:
            print(f"DEBUG:\tno rules matched")
            try:
                token, lexval = next(tokens)
            except StopIteration:
                break
            print(f"DEBUG: have {token=}, {lexval=}")
            stack.shift(Terminal(token, lexval))
    # todo: should check end result is a 'question'
    return stack.result()
