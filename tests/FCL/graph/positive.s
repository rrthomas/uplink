// Everything good in this test.

transition initial -> a;
transition a -> b;
transition b -> c;
transition b -> d;
transition c -> e;
transition c -> f;
transition d -> e;
transition f -> e;
transition e -> terminal;

@initial
toA() {
  transitionTo(:a);
}

@a
toB() {
  myDate = "2018-01-22T13:46:10+00:00";

  between(myDate, myDate + 7d) {
    transitionTo(:b);
  };

}

@b
toCorD() {
  before("2018-01-22T13:46:10+00:00") {
    transitionTo(:c);
  };

  after("2018-01-22T13:46:10+00:00") {
    transitionTo(:d);
  };
}

@c
toEorF() {
  if (True) {
    transitionTo(:e);
  } else {
    transitionTo(:f);
  };
}

@d
toEfromD() {
  transitionTo(:e);
}

@f
toEfromF() {
  transitionTo(:e);
}

@e
finish() {
  terminate("bye");
}
