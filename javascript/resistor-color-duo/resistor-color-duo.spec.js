import { value } from './resistor-color-duo.js';

describe('Resistor Colors', () => {
  test('Brown and black', () => {
    expect(value(['brown', 'black'])).toEqual(10);
  });

  test('Blue and grey', () => {
    expect(value(['blue', 'grey'])).toEqual(68);
  });

  test('Yellow and violet', () => {
    expect(value(['yellow', 'violet'])).toEqual(47);
  });

  test('Orange and orange', () => {
    expect(value(['orange', 'orange'])).toEqual(33);
  });

  test('Ignore additional colors', () => {
    expect(value(['green', 'brown', 'orange'])).toEqual(51);
  })

  // additional
  test('Inexistent colors', () => {
    expect(value(['lime', 'cyan', 'teal'])).toEqual(NaN);
  })

  test('Non-strings', () => {
    expect(value([5, ["a"]])).toEqual(NaN);
  })

  test('Just one color', () => {
    expect(value(['yellow'])).toEqual(NaN);
  })
});
