/* This triggers the error case in the undefinedness analysis, where
   we have nested assignments. Ideally this is caught earlier by the
   type checker, but this also works for now. */

global void a;

transition initial -> terminal;

@initial
init() {
  a = (b = 10);
  terminate("bye");
}
