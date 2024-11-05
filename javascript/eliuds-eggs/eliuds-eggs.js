export const eggCount = (displayValue) => (displayValue.toString(2).match(/1/g) || []).length;
