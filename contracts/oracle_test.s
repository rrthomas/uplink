global float x = 0.0;
transition initial -> get;
transition get -> terminal;
@initial
setX (float y, contract oracle) {
  x = y * contractValue(oracle, "value");
  transitionTo(:get);
}
@get
getX () {
  terminate("Now I die.");
  return x;
}
