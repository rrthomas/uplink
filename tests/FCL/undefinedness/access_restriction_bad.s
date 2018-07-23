global account bob;

transition initial -> terminal;

@initial { bob }
go() { terminate("Bye."); }