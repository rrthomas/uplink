transition initial -> set;
transition set -> terminal;

@initial
manyTypes (
    int a
  , float b
  , msg c
  , account d
  , assetBin e1
  , assetDisc e2
  , assetFrac1 e3
  , contract f
  , sig g
  , datetime h
  , void i
) {
  transitionTo(:set);
}

@set
nothing () {
  terminate("bye");
}
