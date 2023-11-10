export class Triangle {
    constructor(a, b, c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    kind() {
        if ([this.a <= 0
             , this.b <= 0
             , this.c <= 0
             , this.a + this.b < this.c
             , this.a + this.c < this.b
             , this.b + this.c < this.a
            ].some(id)) {
            return null;
        }
        let equal_sides = [this.a === this.b, this.b === this.c, this.a === this.c].filter(id).length;
        return ["scalene", "isosceles", "equilateral", "equilateral"][equal_sides];
    }
    get isScalene() { return this.kind() === "scalene"; }
    get isIsosceles() { return this.kind() === "isosceles" || this.kind() === "equilateral"; }
    get isEquilateral() { return this.kind() === "equilateral"; }
}

const id = (something) => something;
