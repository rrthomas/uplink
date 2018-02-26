/* The following confuses the graph as it silently ignores any
   transition declaration that starts in the terminal state or ends in
   the initial state. */

transition initial -> a;
transition a -> terminal;
transition terminal -> a;

@initial
init() {
  transitionTo(:a);
}

@a
end() {
  terminate("bye");
}

@terminal
revive() {
  transitionTo(:a);
}
