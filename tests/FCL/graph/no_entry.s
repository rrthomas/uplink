/* This test has no initial state, but the initial state is mandatory */

transition a -> terminal;

@a
end() {
  terminate("bye");
}
