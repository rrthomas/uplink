/* The following confuses the graph as it silently ignores any
   transition declaration that starts in the terminal state or ends in
   the initial state. */

transition initial -> a;
transition a -> initial;
transition a -> terminal;

@initial
init() {
  transitionTo(:a);
}

@a
goBack() {
  transitionTo(:initial);
}

@a
end() {
  terminate("bye");
}
