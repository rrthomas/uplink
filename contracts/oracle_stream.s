global date timestamp;
global float value;

transition initial -> end;
transition end -> terminal;

@initial
set(float v) {
  if (sender() == owner()) {
    timestamp = now();
    value = v;
  };
  transitionTo(:end);
}

@end
end() {
  if (sender() == owner()) {
    terminate("This is the end");
  };
}

