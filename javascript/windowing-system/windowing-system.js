// @ts-check

export {Size, Position, ProgramWindow, changeWindow}

function Size(width=80, height=60) {
  this.width = width;
  this.height = height;
}

Size.prototype.resize = function (newWidth, newHeight) {
  this.width = newWidth;
  this.height = newHeight;
};

function Position(x=0, y=0) {
  this.x = x;
  this.y = y;
}

Position.prototype.move = function (newX, newY) {
  this.x = newX;
  this.y = newY;
};

const _procustize = (value, minValue, maxValue) => Math.max(Math.min(value, maxValue), minValue);

class ProgramWindow {
  constructor() {
    this.screenSize = new Size(800, 600);
    this.size = new Size();
    this.position = new Position();
  }
  
  resize(newSize) {
    this.size.width = _procustize(newSize.width, 1, this.screenSize.width - this.position.x);
    this.size.height = _procustize(newSize.height, 1, this.screenSize.height - this.position.y);
    return this;
  }
  
  move(newPosition) {
    this.position.x = _procustize(newPosition.x, 0, this.screenSize.width - this.size.width);
    this.position.y = _procustize(newPosition.y, 0, this.screenSize.height - this.size.height);
    return this;
  }
}

function changeWindow(programWindow) {
  programWindow.resize(new Size(400, 300))
    .move(new Position(100, 150));
  return programWindow;
}
