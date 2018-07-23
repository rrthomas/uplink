local float x;

transition initial -> set;
transition set -> terminal;

@set
end () {
  terminate("Now I die.");
}

@initial
setX () {
  x = x * 3.0;
  transitionTo(:set);
}
