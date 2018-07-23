
global map<account,fixed2> m = {};
global float rate = 0.3;

transition initial -> terminal;

@initial 
f (account a, fixed3 v) {
  m = calcAndInsert(m, a, v);
  terminate("");
}

calcAndInsert(map<account, fixed2> n, account b, fixed2 v) {
  v' = floatToFixed2((fixed2ToFloat(v)) * rate + 26.0);
  v'' = floatToFixed3(fixed2ToFloat(v'));
  mapInsert(b, v'', n); 
}
