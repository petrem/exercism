//
// I wouldn't allow creating an invalid triangle, instead of throwing from kind()...
// but alas, the tests say otherwise.
//

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
      throw new Error("Invalid triangle");
    }
    let equal_sides = [this.a == this.b, this.b == this.c, this.a == this.c].filter(id).length;
    return ["scalene", "isosceles", "equilateral", "equilateral"][equal_sides];
  }
}

const id = (something) => something;
