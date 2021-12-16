import { colorCode } from './resistor-color.js';

export const value = (colorBandArray) => {
    return colorCode(colorBandArray[0]) * 10 + colorCode(colorBandArray[1]);
};
