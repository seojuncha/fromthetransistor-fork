.section .text
.global _start

_start:
  mov r0, #1
  mov r1, #0xff
  mov r3, r0
  mov r4, r1
  mov r5, r3, lsl #1
  mov r6, r4, lsl #3
