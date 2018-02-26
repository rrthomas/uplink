enum trivial { unit };

global int someInt = 0;

transition initial -> terminal;

@initial
run(enum trivial arg) {
  someInt = case(arg) { };
  terminate("Bye");
}
