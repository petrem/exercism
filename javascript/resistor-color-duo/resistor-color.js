// modified version of 'resistor-color' that returns NaN as an error instead of null

export const colorCode = (color) => {
    if (typeof(color) === "string") {
        let value = COLORS.indexOf(color.toLowerCase());
        if (value >= 0)
            return value;
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
