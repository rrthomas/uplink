global account alice;
global account bob = u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc';
global account reallyAlice = u'fwBVDsVh8SYQy98CzYpNPcbyTRczVUZ96HszhNRB8Ve';

transition initial -> next;
transition next -> terminal;

@initial { bob }
init() {
 alice = reallyAlice;
 transitionTo(:next);
}

@next { alice }
go() {
  terminate("Bye.");
}
