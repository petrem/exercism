from typing import Dict, List

LegacyData = Dict[int, List[str]]
Data = Dict[str, int]


def transform(legacy_data: LegacyData) -> Data:
    return {
        letter.lower(): score
        for score, letters in legacy_data.items()
        for letter in letters
    }
