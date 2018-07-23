 
global bool b = False;
global bool f = True;

transition initial -> terminal;

@initial
noeffects() {
  b = readWrite(f);
  tryTransition(); 
  terminate("");
}

// Reads/Writes should not be allowed in helpers
// 'f` read should be allowed (argument reads permitted)
readWrite(bool f) {
  !b && f;
}

// Transitions (write effects) should not be allowed in helpers
// Assignment to 'b' should be disallowed
// read/write of 'x' and 'y' should be allowed 
tryTransition(int x) {
  x = x + 1;
  y = x + 2;
  b = y > 5;
  transitionTo(:terminal);
}
