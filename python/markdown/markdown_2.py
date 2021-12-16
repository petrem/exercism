import re
from abc import abstractmethod, ABCMeta


def parse(markdown):
    pass


class Element(meta=ABCMeta):
    TAG = NotImplemented

    def __init__(self, inner):
        self._inner = inner

    def __str__(self):
        return f"<{self.tag}>{self._inner}</{self.tag}>"

    @property
    def tag(self):
        return self.TAG

    @classmethod
    @abstractmethod
    def parse(cls, s):
        raise NotImplementedError


class Paragraph(Element):
    TAG = "p"

    @classmethod
    def parse(cls, s):
        return cls(s)


class Container(Element):
    pass


class Heading(Container):
    TAG = None
    RE = re.compile(r"(?P<hashes>#|##|######) (?P<title>.*)")

    def __init__(self, level, inner):
        self._level = level
        super().__init__(inner)

    @property
    def tag(self):
        return f"h{self._level}"

    @classmethod
    def parse(cls, s):
        match = re.match(cls.RE, s)
        if match:
            return cls(len(match["hashes"]), match["title"])


class ListItem(Container):
    TAG = "li"
    RE = re.compile(r"\* (?P<item>.*)")

    @classmethod
    def parse(cls, s):
        match = re.match(cls.RE, s)
        if match:
            return cls(match["item"])


# TODO: inners can have tags too (bold, italic)
