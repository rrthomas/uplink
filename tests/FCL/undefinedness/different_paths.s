global int x;

transition initial -> inc;
transition inc -> inc;
transition inc -> terminal;

@initial
setX() {
  x = 10;
  transitionTo(:inc);
}

@initial
dontSetX() {
  transitionTo(:inc);
}

@inc
inc() {
  x = x + 1;
  transitionTo(:inc);
}

@inc
end() {
  terminate("bye");
}
