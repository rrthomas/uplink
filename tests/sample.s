global float x = 0.0;
global fixed3 f = 1.234f;
global fixed2 q;
local int y = 7;
local float v;
asset z = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
contract c = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; 
account a = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

datetime dt;

transition initial -> setup;
transition setup -> confirmation;
transition confirmation -> settlement;
transition initial -> getX;
transition setX -> getX;
transition getX -> setX;
transition settlement -> terminal;

@setDate
setDate() {
  dt = "2020-10-20T15:50:12+00:00";
}

@initial
initialize () {
  transitionTo(:getX);
}

@setX
setX (int j, float k) {
  x = k;
  y = y * j;
  f = 2.516f + f;
  x = fixed3ToFloat(floatToFixed3(k)) + x;
  transitionTo(:getX);
  return x;
}

@getX
getX () {
  j = 10 + 7 * 10;
  k = j;
  l = k;
  m = 1.23f + 4.56f - 7.89f * 9.87f / 65.43f;
  q = m + 1.00f + floatToFixed2(x);
  transitionTo(:setX);
  return q;
}

@f
f (int j, bool k) { 
  if (k) {
    return j;
  } else {
    return -1;
  };
}

@g
g (asset f, account t) {
  if (assetExists(f) && accountExists(t)) {
    transferTo(f, 20);
    transferFrom(f, 20, t);
  } else {
    return void; 
  };
}
