enum duplication { Foo, Bar };

enum duplication { Zig, Zag };

transition initial -> terminal;

@initial
run() {
  terminate("Bye");
}
