.section .text
.global _start

_start:
  mov r0, #1  @ r0 = 1
  mov r1, #2  @ r1 = 2
  
  add r2, r0, #3   @ r2 = 1 + 3 = 4
  add r3, r1, r2   @ r3 = 2 + 4 = 6

  sub r2, r3, #4  @ r2 = 2
