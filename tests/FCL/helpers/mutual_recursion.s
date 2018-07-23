enum flipFlop
  { Flip
  , Flop
  };

global enum flipFlop flipper;

transition initial -> terminal;

@initial 
flipeeFlopee(enum flipFlop flopper) {
  flipper = flip(flopper);
  terminate("flip was flopped");
}

flip (enum flipFlop flipee) {
  case(flipee) {
    `Flip -> flop(`Flop);
    `Flop -> `Flip;
  };
}

flop (enum flipFlop flopee) {
  case(flopee) {
    `Flip -> `Flop;
    `Flop -> flip(`Flip);
  };
}
