enum myEnum
  { Hello
  , There
  };

global enum myEnum hiEnum = `Hello;
global int foo = 10;

transition initial -> terminal;

@initial
run(enum myEnum val) {
  foo = case(val) {
            `Hello -> 1;
	    `There -> 2;
	    };

  terminate("bye");
}
