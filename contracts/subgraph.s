transition initial -> middle;
transition middle -> terminal;

transition initial -> novate_x;

// Main sequence

@initial
entry() {
  transitionTo(:middle);
}

@middle
middle () {
  transitionTo(:terminal);
}

// Subgraph novation over x

@@initial
start () {
  novationInit(32);
  transitionTo(:novate_x);
}

@@novate_x 
stop () {
  novationStop();
}
