
global int a = 5;

transition initial -> terminal;

@initial
shadow (int a) {
  a = a + 5;
  terminate("shadowed");
}
