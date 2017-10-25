global int x = 0 ;

transition initial -> get;
transition get -> terminal;

@get
getX () {
  terminate("Now I die.");
  return x;
}

@initial
setX () {
  x = 42;
  transitionTo(:get);
  return void;
}
