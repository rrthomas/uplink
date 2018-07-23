transition initial -> terminal;
transition terminal -> initial; // should be disallowed

@initial
f () { transitionTo(:terminal); }

@terminal
g () { transitionTo(:initial); }