/* This contract is in "practice" valid, in that calling init() and
   then notCrash() does not give us any problems. But since we
   probably do not want to any analysis on the conditions in
   if-statements, the analysis should assume that both cases are
   possible and should consider both branches as reachable. If there
   is at least one branch that refers to an uninitialised variable,
   then the analysis should reject the program.
*/

global int a;
global bool cond;

transition initial -> stateA;
transition stateA -> terminal;

@initial
init() {
  cond = False;
  transitionTo(:stateA);
}

@stateA
notCrash() {
  if (cond) {
    sha256(a);
    terminate("whoops");
  } else {
    terminate("bye");
  };
}
