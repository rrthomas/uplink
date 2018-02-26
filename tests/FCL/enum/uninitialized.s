enum direction { Left, Right };

global int i;

transition initial -> assigned;
transition initial -> unassigned;
transition assigned -> assigned;
transition assigned -> terminal;
transition unassigned -> assigned;
transition unassigned -> unassigned;

@initial
start(enum direction dir) {
  case(dir) {
    `Left -> { i = 10; transitionTo(:assigned); };
    `Right -> transitionTo(:unassigned);
  };
}

@assigned
increase() {
  i = i + 1;
  transitionTo(:assigned);
}

@assigned
assignedStop() {
  terminate("Bye");
}

@unassigned
assign(enum direction dir) {
  case(dir) {
    `Left -> { i = 10; transitionTo(:assigned); };
    `Right -> transitionTo(:unassigned);
  };
}
