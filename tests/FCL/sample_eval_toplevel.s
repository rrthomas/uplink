global datetime a = "2020-01-01T13:33:37+00:00";
global datetime b = a + 10d;
global datetime c;

transition initial -> terminal;

@initial
init() {
  c = b + 20d;
  terminate("Bye");
}
