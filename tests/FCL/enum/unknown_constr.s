enum trivial { unit };

global int someInt = 0;

transition initial -> terminal;

@initial
run(enum trivial arg) {
  someInt = case(arg) {
   `weird -> 10;
   `unit -> 20;
  };

  terminate("Bye");
}
