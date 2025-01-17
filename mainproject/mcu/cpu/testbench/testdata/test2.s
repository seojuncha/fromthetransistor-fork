.section .text
.global _start

_start:
  mov r0, #2      @ e3a00002, r0 = 2
  add r1, r0, #1  @ e2801001, r1 = 3
  sub r2, r0, #1  @ e2402001, r2 = 1
