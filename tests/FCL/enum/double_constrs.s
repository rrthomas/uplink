enum duplication { Foo, Bar, Foo };

transition initial -> terminal;

@initial
run() {
  terminate("Bye");
}
