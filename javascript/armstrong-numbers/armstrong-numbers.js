export const isArmstrongNumber = (num) => {
    let digits = [...String(num)].map(Number);
    let sum = digits.reduce((acc, d) => acc + d**digits.length, 0);
    return num === sum;
};
