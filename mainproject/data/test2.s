.section .text
.global _start

_start:
  mov r0, #0x00011000   @ r0 = 0x0001_1000
  mov r1, #42           @ r1 = 42
  str r1, [r0]          @ memory[0x0001_1000] <= 42

  ldr r2, [r0]          @ r2 <= memory[0x0001_1000] = 42
  add r2, r2, #10       @ r2 = 52
  str r2, [r0, #4]      @ memory[0x0001_1000+4] = memory[0x0001_1004] <= 52

  ldr r3, [r0, #4]      @ r3 = 62
  mov r7, #1            @ r7 = 1
  mov r0, #0            @ r0 = 0
