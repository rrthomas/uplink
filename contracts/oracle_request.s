global date timestamp;
global float value;
global bool requesting;
global date requested_timestamp;

transition initial -> requesting;
transition requesting -> initial;
transition initial -> terminal;
transition requesting -> terminal;

@initial
set(date ts, float v) {
  if ((sender() == owner()) && (requesting == True)) {
    tmestamp = ts;
    value = v;
    requesting = False;
  };
}

@requesting
request(date dt) {
  if ((sender() == owner()) && (requesting == False)) {
    requested_timestamp = dt;
    requesting = True;
  }
}
@end
end() {
  if (sender() == owner()) {
    terminate("This is the end");
  };
}

