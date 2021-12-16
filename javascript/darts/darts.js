export const solve = (x, y) => {
    if (Number.isNaN(Number(x)) || Number.isNaN(Number(y))) return null;
    let d_squared = x*x + y*y;
    let scores = [
        {"radius": 1, "score": 10}
        ,{"radius": 5, "score": 5}
        ,{"radius": 10, "score": 1}
        ,{"radius": Number.MAX_SAFE_INTEGER, "score": 0}
    ];
    return scores.find((x) => {return d_squared <= x.radius * x.radius}).score;
};
