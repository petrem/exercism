"use strict";

const alphabet = "abcdefghijklmnopqrstuvwxyz";

export const isPangram = (thing) => {
    let lowerThing = thing.toLowerCase();
    return [...alphabet].every((chr) => lowerThing.includes(chr));
};


// Other solutions
export const isPangram1 = (thing) => {
    let checklist = new Set("abcdefghijklmnopqrstuvwxyz");
    [...thing.toLowerCase()].forEach( (item, index, array) => { checklist.delete(item); } );
    return checklist.size == 0;
};

export const isPangram2 = (thing) => {
    let checklist = new Set();
    [...thing.toLowerCase()]
        .filter((chr) => /[a-z]/.test(chr))
        .reduce((acc, chr) => acc.add(chr), checklist );
    return checklist.size == 26;
};


