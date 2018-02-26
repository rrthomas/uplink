/* This contract may never reach the terminal state even though the
   transitions pass the completeness and soundness checks. */

transition initial -> good;
transition good -> terminal;
transition good -> bad;
transition bad -> bad;

@initial
init() {
  transitionTo(:good);
}

@good
ok() {
  terminate("bye");
}

@good
notOk() {
  transitionTo(:bad);
}

@bad
loop() {
  transitionTo(:bad);
}
