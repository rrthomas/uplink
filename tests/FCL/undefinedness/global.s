/* This crashes at run-time when calling init() */
global int a;

transition initial -> terminal;

@initial
init() {
  z = sha256(a);
  terminate("bye");
}
