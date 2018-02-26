enum empty { };

transition initial -> terminal;

@initial
run() {
  terminate("Bye");
}
