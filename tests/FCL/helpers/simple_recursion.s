
global int x = 0;

transition initial -> terminal;

@initial 
callHelper(int y) {
  x = addOneForever(y);  
  terminate("");
}

addOneForever(int y) {
  addOneForever(y + 1);
}
