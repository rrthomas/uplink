global float x = 0.0;
global fixed3 f = 1.234f;
global fixed2 q;
local int y = 7;
local float v;
assetFrac5 z = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
contract c = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; 
account a = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

datetime dt;

transition initial -> setX;
transition setX -> update;
transition update -> setX;
transition setX -> setup;
transition update -> setup;
transition setup -> confirmation;
transition confirmation -> settlement;
transition settlement -> terminal;

transition initial -> circulated;
transition circulated -> terminal;

@setDate
setDate() {
  dt = "2020-10-20T15:50:12+00:00";
}

@initial
initialize () {
  transitionTo(:setX);
}

@setup
confirm () {
  transitionTo(:confirmation);
}

@confirmation
settle () {
  transitionTo(:settlement);
}

@settlement
finalize () {
  transitionTo(:terminal);
}

@setX
setX (int j, float k) {
  x = k;
  y = y * j;
  f = 2.516f + f;
  x = fixed3ToFloat(floatToFixed3(k)) + x;
  transitionTo(:update);
}

@setX
fixX () {
  transitionTo(:setup);
}

@update
fixY () {
  transitionTo(:setup);
}

@update
update () {
  j = 10 + 7 * 10;
  k = j;
  l = k;
  m = 1.23f + 4.56f - 7.89f * 9.87f / 65.43f;
  q = m + 1.00f + floatToFixed2(x);
  transitionTo(:setX);
}

@f
f (int j, bool k) { 
  if (k) {
  } else {
  };
}



@g
g (assetDisc f, account t) {
  if (assetExists(f) && accountExists(t)) {
    transferTo(f, 20);
    transferFrom(f, 20, t);
  };
}

@initial
circulate(assetFrac2 a, fixed2 amount) {
  circulate(a,amount);
  transitionTo(:circulated);
}

@circulated
transfer(assetBin a, account from, account to, bool amount) {
  transferHoldings(from,a,amount,to);
  terminate("finished transfer");
}

