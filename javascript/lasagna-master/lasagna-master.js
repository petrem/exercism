/// <reference path="./global.d.ts" />
// @ts-check

export function cookingStatus(timer) {
    if (timer === undefined) {
        return "You forgot to set the timer.";
    } else if (Number(timer) > 0) {
        return "Not done, please wait.";
    } else if (Number(timer) === 0) {
        return "Lasagna is done.";
    } else {
        throw new Error(`I wasn't told what to do with ${timer}`);
    }
}

export function preparationTime(layers, averageDuration = 2) {
    return layers.length * Number(averageDuration);
}

export function quantities(layers = []) {
    return layers.reduce((acc, layer) => {
        switch(layer) {
        case "noodles":
            acc["noodles"] += 50;
            break;
        case "sauce":
            acc["sauce"] += 0.2;
            break;
        }
        return acc;
    }, {noodles: 0, sauce: 0});
}

export function addSecretIngredient(theirList, myList) {
    myList.push(theirList.at(-1));
}

export function scaleRecipe(recipe, portions) {
    const factor = portions / 2;
    let scaledRecipe = {};
    for (let key in recipe) {
        scaledRecipe[key] = recipe[key] * factor;
    }
    return scaledRecipe;
}
