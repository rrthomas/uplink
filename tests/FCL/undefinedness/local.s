/* We assume local variables to be initialised, so the following is
   accepted, but unsafe.
*/

local int a;

transition initial -> terminal;

@initial
init() {
  z = sha256(a);
  terminate("bye");
}
