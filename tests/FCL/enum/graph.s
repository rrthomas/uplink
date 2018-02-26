enum direction { Left, Right };

transition initial -> a;
transition a -> b;
transition a -> c;
transition b -> d;
transition c -> d;
transition d -> terminal;

@initial
start() {
  transitionTo(:a);
}

@a
leftOrRight(enum direction dir) {
  case(dir) {
    `Left -> transitionTo(:b);
    `Right -> transitionTo(:c);
  };
}

@b
fromBtoD() {
  transitionTo(:d);
}

@c
fromCtoD(enum direction dir) {
  case (dir) {
    `Left -> transitionTo(:d);
    `Right -> transitionTo(:d);
  };
}

@d
stop() {
  terminate("Bye");
}
