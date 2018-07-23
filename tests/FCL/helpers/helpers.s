
global map<account,fixed2> m = {};

transition initial -> terminal;

@initial 
f (account a, fixed2 v) {
  m = calcAndInsert(m, a, v);
  terminate("");
}

addFee(fixed2 x) { x + 123.45f; }

calcTotal(fixed2 x) {
  rate = 0.3;
  x' = fixed2ToFloat(addFee(x));
  x' + x' * rate;
}

calcAndInsert(map<account, fixed2> n, account b, fixed2 v) {
  v' = floatToFixed2(calcTotal(v)); 
  mapInsert(b, v', n); 
}
