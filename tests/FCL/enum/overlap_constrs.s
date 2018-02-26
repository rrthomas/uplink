enum enumOne { Foo, Bar };

enum enumTwo { Bar, Baz };

transition initial -> terminal;

@initial
run() {
  terminate("Bye");
}
