global int x = 5;
local int y = 7;
local int z = 3;

transition initial -> f;

@initial
f () {
  y = y + z;    // Add two VCrypto, y = 10    
  z = x * z;    // Multiply a VCrypto with temp scalar, z = 15       
  y = z - y;    // Subtract two VCrypto, y = 5
}
