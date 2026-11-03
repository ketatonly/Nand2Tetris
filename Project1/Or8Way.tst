<<<<<<< HEAD
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/1/Or8Way.tst

load Or8Way.hdl,
compare-to Or8Way.cmp,
output-list in%B2.8.2 out;

set in %B00000000,
eval,
output;

set in %B11111111,
eval,
output;

set in %B00010000,
eval,
output;

set in %B00000001,
eval,
output;

set in %B00100110,
eval,
=======
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/1/Or8Way.tst

load Or8Way.hdl,
compare-to Or8Way.cmp,
output-list in%B2.8.2 out;

set in %B00000000,
eval,
output;

set in %B11111111,
eval,
output;

set in %B00010000,
eval,
output;

set in %B00000001,
eval,
output;

set in %B00100110,
eval,
>>>>>>> 1409a0aa7b55f0cecd900b116671920d3cd641a6
output;