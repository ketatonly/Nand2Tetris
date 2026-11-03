// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.

//// Replace this comment with your code.
  
(MAIN_LOOP)
  // 1. Check the keyboard
  @KBD
  D=M
  @CLEAR_SCREEN
  D;JEQ      // If D == 0 (no key pressed), jump to CLEAR_SCREEN

  // 2. If we are here, a key IS pressed. Fall through to FILL.
  @color
  M=-1       // Set color to -1 (black)
  @DRAW
  0;JMP      // Jump to DRAW

(CLEAR_SCREEN)
  @color
  M=0        // Set color to 0 (white)

(DRAW)
  // 3. Setup the pointer and counter
  @SCREEN
  D=A        // D = 16384 (the address of the first screen pixel)
  @ptr
  M=D        // ptr = 16384

  @8192
  D=A        // D = 8192 (total number of 16-bit words on the screen)
  @count
  M=D        // count = 8192

(DRAW_LOOP)
  // 4. Write the color to the screen
  @color
  D=M        // D = the color (-1 or 0)
  
  @ptr
  A=M        // A = the current screen address (ptr)
  M=D        // Write the color to RAM[A]

  // 5. Move to the next word and decrease counter
  @ptr
  M=M+1      // ptr = ptr + 1
  
  @count
  M=M-1      // count = count - 1

  // 6. Check if we are done
  @count
  D=M
  @MAIN_LOOP
  D;JEQ      // If count == 0, we are done drawing. Go back to MAIN_LOOP.

  // 7. If we are not done, loop back to DRAW_LOOP
  @DRAW_LOOP
  0;JMP


  