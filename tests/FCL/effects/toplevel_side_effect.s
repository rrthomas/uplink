global account myAccount = sender();
global void mySideEffect = transitionTo(:terminal);

transition initial -> terminal;

@initial
go() {
  terminate("Bye");
}
