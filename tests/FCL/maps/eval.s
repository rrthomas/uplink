
enum role
  { BigInvestor
  , SmallInvestor
  };

global map<account, int> shares = {};
global map<account, int> runoff = {};

transition initial -> terminal;

@initial
insertInvestor (account a, enum role x)  {
  shares = case(x) {
    `BigInvestor -> mapInsert(a, 100, shares);
    `SmallInvestor -> mapInsert(a, 10, shares);
  };
}

@initial
deleteInvestor (account a) {
  shares = mapDelete(a, shares);
  runoff = mapInsert(a, 10, runoff);
}

@initial 
lookupInvestor (account a) { 
  v = lookup(a,shares);
  runoff = mapInsert(a, v + 10, shares);
}

@initial
rem100Shares (account a) {
  if (sender() == a) {
    shares = modify(a, subtract100, shares); 
    runoff = modify(a, add100, runoff); 
  };
}

@initial
end () { terminate(""); }

subtract100(int v) { 
  rem = v - 100;
  if (rem >= 0) { rem; } else { 0; };
}

add100(int v) {
  v + 100;
}
