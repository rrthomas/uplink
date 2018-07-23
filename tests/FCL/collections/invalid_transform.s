
global map<account, float> balances = 
  { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1.0
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2.0
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3.0
  };

global map<account, int> balances' = 
  { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3
  };

transition initial -> terminal;

@initial
applyInterest() {
  balances = transform(calcInterest, balances);
  transitionTo(:terminal);
}

@initial 
applyInterest'() {
  balances = transform(calcInterest, balances'); 
}

// double the interest?
calcInterest(int balance) {
  balance * 2 + balance;
}
