global int a;
global int b;
global int c;

transition initial -> stateA;
transition initial -> stateB;
transition stateB -> stateB;
transition stateB -> stateA;
transition stateA -> terminal;

/* The following sequence of method calls causes a crash:
     - init()
     - goA()
     - initA()

   The following does not:
     - init()
     - goB()
     - initB()
     - initA()

*/

@initial
init() {
  a = 1;
}

@initial
goA() {
  transitionTo(:stateA);
}

@initial
goB() {
  transitionTo(:stateB);
}

@stateB
initB() {
  b = 2;
  transitionTo(:stateA);
}

@stateB
loop() {
  transitionTo(:stateB);
}

@stateA
initA() {
  c = a + b;
  terminate("finished");
}
