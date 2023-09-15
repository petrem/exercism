import pytest

from roman_numerals import (
    roman,
)


def test_7_is_vii():
    assert roman(7) == "VII"

def test_70_is_cxx():
    assert roman(70) == "LXX"

def test_73_is_cxxiii():
    assert roman(73) == "LXXIII"

def test_8_is_viii():
    assert roman(8) == "VIII"

def test_80_is_cxxx():
    assert roman(80) == "LXXX"
