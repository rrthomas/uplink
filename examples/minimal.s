global int x = 0 ;

transition initial -> set;
transition set -> terminal;

@set
end () {
  terminate("Now I die.");
}

@initial
setX () {
  x = 42;
  transitionTo(:set);
}
