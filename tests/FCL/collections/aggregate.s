
global int totalMap = 0;
global int numMembers = 0;

global map<account, int> balances = 
  { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3
  };

global set<account> members = 
  ( u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve' 
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc'
  );

transition initial -> terminal;

@initial
sumBalances() {
  totalMap = aggregate(totalMap,sum,balances);
  numMembers = aggregate(0, countMember, members);  
  transitionTo(:terminal);
}

sum (int x, int y) { x + y; }
sumPlusOne (int x, int y) { 
  sum(x,y) + 1;
}

countMember(int n, account a) { n + 1; }

