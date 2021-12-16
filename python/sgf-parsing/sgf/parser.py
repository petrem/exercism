from abc import ABC, abstractmethod


class Parser(ABC):
    @abstractmethod
    def parse(s:str):
        raise NotImplementedError

    def match():
        pass
