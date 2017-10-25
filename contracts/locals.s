local float x;

transition initial -> get;
transition get -> terminal;

@get
getX () {
  terminate("Now I die.");
  return x;
}

@initial
setX () {
  x = x * 3.0;
  transitionTo(:get);
  return void;
}
