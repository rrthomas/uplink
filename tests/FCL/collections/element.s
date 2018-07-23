
// All bool values should be "true" 

global map<account, int> balances = {};
global bool aElemOf;
global bool notElemOf;

global set<int> amounts = (1,2,3);
global bool elemOfSet;
global bool notElemOfSet;

transition initial -> terminal;

@initial
elementTest(account a, int amount) {
  // tests for element of map
  balances = mapInsert(a, amount, balances);
  aElemOf = element(amount, balances);
  notElemOf = !element(amount - 1, balances);
 
  // tests for element of set
  amounts = setInsert(amount, amounts);
  elemOfSet = element(amount, amounts);
  notElemOfSet = !element(0, amounts);

  terminate("end elementTest");
}
