global int x;
global int y;
global bool yDone = False;
global bool xDone = False;

transition initial -> set;
transition set -> set;
transition set -> get;
transition get -> terminal;

@initial
entry() {
  transitionTo(:set);
}

@set
setX () {
  x = 10;
  xDone = True;
  if (yDone) {
    transitionTo(:get);
  } else {
    transitionTo(:set);
  };
}

@set
setY () {
  y = 20;
  yDone = True;
  if (xDone) {
    transitionTo(:get);
  } else {
    transitionTo(:set);
  };
}

@get
getZ () {
  z = x+y;
  terminate("Now I die");
  return z;
}
