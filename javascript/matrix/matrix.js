export class Matrix {
  constructor(str) {
    this._rows = str.replace("\r", "")
      .split("\n")
      .map((row) => row.split(" ").map((x) => parseInt(x, 10)));
    let n_cols = this._rows[0].length;
    if (!(this._rows.every((row) => row.length == n_cols))) {
      throw new Error("Matrix should have same number of columns on each row");
    }
  }

  get rows() {
    return this._rows;
  }

  get columns() {
    return zip(this._rows);
  }
}

const zip = (rows) => rows[0].map((_, idx) => rows.map((row) => row[idx]));
