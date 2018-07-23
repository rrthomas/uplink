
enum role
  { BigInvestor
  , MedInvestor
  , SmallInvestor
  };

global map<enum role, set<account>> investors = 
  { `BigInvestor : ()
  , `MedInvestor : ()
  , `SmallInvestor : ()
  };

transition initial -> terminal;

@initial 
insertInvestor(account a, enum role x) {
  currSet = lookup(x, investors);
  newSet = 
    if (!element(a, currSet)) {
      setInsert(a, currSet);
    } else {
      setInsert(a, ()); 
    };
  investors = mapInsert(x, newSet, investors); 
}

@initial 
deleteInvestor(account a, enum role x) {
  currSet = lookup(x, investors);
  newSet =
    if (element(a, currSet)) {
      setDelete(a, currSet);
    } else {
      currSet;
    };
  investors = mapInsert(x, newSet, investors); 
}

@initial
end() { terminate(""); }
