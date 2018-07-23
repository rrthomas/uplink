
global map<account, int> balances = 
  { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1
  , u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2
  , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3
  , u'2R5XggBkMCQwh74KuqBh553jzCpY9ma7PmkPTBP8ti6j' : 4
  , u'AAqhpDc5dabugYAemzc6DVtSyiC5RMoz5MiPdZBMrf7m' : 5
  };

global set<map<account, int>> groups = 
  ( { u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65' : 1 }
  , { u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve'  : 2
    , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' : 3
    , u'2R5XggBkMCQwh74KuqBh553jzCpY9ma7PmkPTBP8ti6j' : 4
    }
  , { u'AAqhpDc5dabugYAemzc6DVtSyiC5RMoz5MiPdZBMrf7m' : 5
    , u'2R5XggBkMCQwh74KuqBh553jzCpY9ma7PmkPTBP8ti6j' : 6
    }
  );

transition initial -> terminal;

@initial
filterTest() {
  balances = filter(moreThanTwo, balances);
  groups   = filter(anyMoreThanTwo, groups);
  terminate("");
}

moreThanTwo(int v) {
  v > 2;
}

anyMoreThanTwo(map<account, int> bals) {
  !isEmpty(filter(moreThanTwo, bals));
}
