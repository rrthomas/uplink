global sig signature;

transition initial -> set;
transition set -> terminal;

@initial
put(msg x) {
  if (sender() == deployer()) {
    signature = sign(x);
    transitionTo(:set);
  };
}

@set
end() {
  terminate("This is the end.");
}
