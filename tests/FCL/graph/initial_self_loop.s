global bool g = False;

transition initial -> initial;
transition initial -> terminal;

@initial
go() {
  if (g) { terminate("Bye."); }
  else { transitionTo(:initial); };
}