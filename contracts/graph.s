global int x;
global int y;

transition initial -> xIsSet;
transition initial -> yIsSet;
transition xIsSet -> bothSet;
transition yIsSet -> bothSet;
transition bothSet -> terminal;

@initial
setX() {
  x = 10;
  transitionTo(:xIsSet);
}

@yIsSet
setX2() {
  x = 10;
  transitionTo(:bothSet);
}

@initial
setY() {
  y = 20;
  transitionTo(:yIsSet);
}

@xIsSet
setY2() {
  y = 20;
  transitionTo(:bothSet);
}

@bothSet
setZ () {
  z = x+y;
  terminate("Now I die");
}
