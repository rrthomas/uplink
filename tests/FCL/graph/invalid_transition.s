transition initial -> terminal;

@initial
init() {
  transitionTo(:bad);
  terminate("bye");
}
