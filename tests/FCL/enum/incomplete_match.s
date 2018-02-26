enum items { First, Second, Third };

global int i = 0;

transition initial -> terminal;

@initial
foo(enum items item) {
  i = case(item) {
        `First -> 1;
	`Second -> 2;
      };
  terminate("Bye");
}

  
