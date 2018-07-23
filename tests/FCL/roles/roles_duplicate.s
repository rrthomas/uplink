global account admin = u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc';
global account bob = u'CGDNCiCEWP3P5jHtrvgphBc6pxGdGG6nQP3VoCW7HoGk';

transition initial -> terminal;

@initial { u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' // ok
         , u'6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc' // bad
         , deployer() // ok
         , deployer() // bad
         , admin // ok
         , admin // bad
         , admin // bad
         , bob // ok
         }
init() {
 
  terminate("Bye.");
}