
global map<account, int> shares = {};

transition initial -> terminal;

@initial
insertTest(account a, datetime d, fixed6 amount) {
  mapInsert(d, 5, shares);
  mapinsert(a, amount, shares);
  mapinsert(a, 500, shares);
  terminate("inserted");
}

@initial
deleteTest(assetFrac5 a) {
  mapDelete(a, shares);
  terminate("deleted");
}

@initial
lookupTest(msg m) {
  lookup(m,shares);
  terminate("lookedup");
}
