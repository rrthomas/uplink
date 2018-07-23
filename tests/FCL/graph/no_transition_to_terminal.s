/* This contract neither declares nor implements a path to a terminal state.
   This catches a corner case where previously we didn't check terminal state
   reachability when no transition to a terminal state was declared.
*/

transition initial -> a;

@initial
init() {
  transitionTo(:a);
}

@a
doesNotTerminate() { }