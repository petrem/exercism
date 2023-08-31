export const value = (colorBandArray) => {
    return colorCode(colorBandArray[0]) * 10 + colorCode(colorBandArray[1]);
};

// modified version of 'resistor-color' that returns NaN as an error instead of null

export const colorCode = (color) => {
    if (typeof(color) === "string") {
        let val = COLORS.indexOf(color.toLowerCase());
        if (val >= 0)
            return val;
    }
    return NaN;
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
