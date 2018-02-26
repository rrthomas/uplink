global int x;
global int y;
global int z;

transition initial -> a;
transition a -> b;
transition b -> c;
transition c -> d;
transition d -> a;
transition b -> terminal;

@initial
start() {
 x = 10;
 transitionTo(:a);
}

@a
one() {
 y = 20;
 transitionTo(:b);
}

@b
two() {
 z = 30;
 transitionTo(:c);
}

@c
three() {
  z = 10;
  transitionTo(:d);
}

@d
four(int w) {
  x = x + w;
  y = y + w;
  z = z + w;
  transitionTo(:a);
}

@b
exit() {
  terminate("bye");
}
