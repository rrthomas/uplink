transition initial -> terminal;

@initial
run(enum nonExisting arg) {
  terminate("Bye");
}
