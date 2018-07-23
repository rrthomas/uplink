global int x = 0;
local float y = 0.0;
assetFrac2 z = a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
contract c = c'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; 
account a = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

transition initial -> update;
transition update -> f;
transition f -> g;
transition g -> terminal; 

@initial
setX (int q) {
  x = 42;
  y = q;
  transitionTo(:update);
}

@update
update () {
  j = 10 + 7 * 1.0;
  k = j;
  l = k;
  y = y + 7.0;
  q = 1.23f + 523.234f * 1.23456f;
  transitionTo(:f);
}

@f
f (int j, bool k) {
  transfer(a,10,z);
  accountExists(z);
  assetExists(a);
  transitionTo(:g);
}

@g
g (assetBin f, account t) {
  if (assetExists(t) && accountExists(f)) {
    transfer(t,f,20);
  };
  terminate("end");
}
