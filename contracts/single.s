global int x = 0 ;

transition initial -> terminal;

@initial
entry () {
  terminate("Now I die.");
  return x;
}
