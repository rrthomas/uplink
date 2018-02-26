/* Not all states are reachable. */

transition initial -> good;
transition good -> terminal;
transition bad -> terminal;

@initial
init() {
  transitionTo(:good);
}

@good
ok() {
  terminate("bye");
}

/* We cannot reach this state / call this method. */
@bad oops() {
  terminate("whoops");
}
