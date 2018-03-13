global datetime a;
global datetime b = a + 10d;

transition initial -> terminal;

@initial
foo() {
  terminate("Bye");
}
