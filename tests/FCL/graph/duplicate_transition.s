transition initial -> a;
transition a -> b;
transition a -> b;
transition b -> terminal;

@initial
start() {
  transitionTo(:a);
}

@a
go() {
  transitionTo(:b);
}

@b
end() {
  terminate("Bye");
}
