// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// The algorithm is based on repetitive addition.

//// Replace this comment with your code.

@mask
M=1 // Initialize the mask to 1
@R2
M=0 // Initialize the result to 0

@R0
D=M
@multiplicand
M=D // Initialize the multiplicand to the initial R0

(loop)
@R1
D=M
@mask
D=D-M
@END
D;JLT // if the mask is bigger than the multiplicator then we are finished

@R1
D=M
@mask
D=D&M
@noAdd
D;JEQ // If the current bit in R1 is 0, skip the addition

@multiplicand
D=M
@R2
D=D+M
M=D // Adding the doubled multiplicator to the result

(noAdd)
@multiplicand
D=M
D=D+M
M=D // Doubling the multiplicand

@mask
D=M
D=D+M
M=D // DOubling the mask
@loop
0;JMP // jumping back to the loop beginning 


(END)
@END
0;JMP





