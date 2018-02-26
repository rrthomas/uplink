enum enumOne { Foo, Bar };

transition initial -> terminal;

@initial
run(enum enumOne arg) {
  case(arg) {
    `Foo -> terminate("Foo");
    `Bar -> terminate("Bar");
    `Foo -> terminate("Foo again");
  };
}
