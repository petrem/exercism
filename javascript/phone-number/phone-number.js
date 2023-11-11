export const clean = (number) => {
    let cleaned = [...number].reduce(function (acc, x) {
        if (!isNaN(Number.parseInt(x))) {
            return acc.concat(x);
        }
        if ([..."+ -.()"].indexOf(x) > -1) {
            return acc;
        }
        if (x.match(/[a-z]/i)) {
            throw new Error('Letters not permitted');
        }
        throw new Error('Punctuations not permitted');
    }, "");
    if (cleaned.length < 10) {
        throw new Error("Incorrect number of digits");
    }
    let cleaned10;
    if (cleaned.length === 11) {
        if (cleaned[0] !== "1") {
            throw new Error("11 digits must start with 1");
        }
        cleaned10 = cleaned.slice(1);
    } else {
        cleaned10 = cleaned;
    }
    if (cleaned.length > 11) {
        throw new Error("More than 11 digits");
    }
    validateComponent(cleaned10.slice(0, 3), "Area");
    validateComponent(cleaned10.slice(3, 6), "Exchange");
    
    return cleaned10;
};


let validateComponent = (component, name) => {
    if (component.startsWith(0) || component.startsWith(1)) {
        let digitName = Number.parseInt(component[0]) === 0 ? "zero" : "one";
        throw new Error(`${name} code cannot start with ${digitName}`);
    }
}
