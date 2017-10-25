global sig signature;
global bool isSet = False;

transition initial -> get;
transition put -> get;
transition get -> terminal;

@initial
put(msg x) {
  if ( !isSet ) {
      isSet = True;
      signature = sign(x);
      transitionTo(:get);
  } else { 
    return void;
  };
}

@get
isSigAvail() {
  return isSet;
}

@get
get() {
  if (isSet) {
    return signature;
  } else {
    terminate("This is the end.");
  };
}
