global int x = 0;
local float y = 0.0;
asset z = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
contract c = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; 
account a = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

@setX
setX (int z) {
  x = 42;
  y = z;
  return y;
}

@getX
getX () {
  j = 10 + 7 * 1.0;
  k = j;
  l = k;
  y = y + 7.0;
  q = 1.23f + 523.234f * 1.23456f;
  return l;
}

@f
f (int j, bool k) {
  transfer(a,10,z);
  accountExists(z);
  assetExists(a);
  return k; 
}

@g
g (asset f, account t) {
  if (assetExists(t) && accountExists(f)) {
    transfer(t,f,20);
  } else {
    return 5;
  };
}
