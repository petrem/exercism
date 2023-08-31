export const decodedValue = (colorBandArray) => {
    return colorCode(colorBandArray[0]) * 10 + colorCode(colorBandArray[1]);
};

// modified version of 'resistor-color' that returns NaN as an error instead of null

export const colorCode = (color) => {
  let val = COLORS.indexOf(color.toLowerCase());
  return val >= 0 ? val : NaN;
};


export const COLORS = [
    "black"
    , "brown"
    , "red"
    , "orange"
    , "yellow"
    , "green"
    , "blue"
    , "violet"
    , "grey"
    , "white"
];
