enum flipFlop
  { Flip
  , Flop
  };

global int x = 0;
global enum flipFlop flipper;

transition initial -> intermediate;
transition intermediate -> terminal;

@initial 
flipeeFlopee(enum flipFlop flopper) {
  flipper = flip(flopper);
  transitionTo(:intermediate);
}

@intermediate 
setX(int y) {
  x = y + incIfLessThan100(x);
  transitionTo(:terminal);
}

// Flip from `Flop to `Flip
flip (enum flipFlop flipee) {
  case(flipee) {
    `Flip -> `Flip;
    `Flop -> `Flip;
  };
}

// Flip from `Flop to `Flip and `Flip to Flop
flipFlop (enum flipFlop flopee) {
  case(flopee) {
    `Flip -> `Flop;
    `Flop -> flip(`Flop);
  };
}

addOne(int z) { z + 1; }

incIfLessThan100(int z) { 
  if (z >= 100) {
    z;    
  } else {
    addOne(z); 
  };
}
