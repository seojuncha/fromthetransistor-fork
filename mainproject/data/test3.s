.section .text
.global _start

_start:
  mov r0, #5    @ r0 = 5
  mov r1, #0x00010000   @ r1 = 0x10000 = 65536
  str r0, [r1]    @ mem[0x10000] = 5

  mov r2, #100    @ r2 = 100
  str r2, [r1, #4]    @ mem[0x10004] = 100
  b next_step

ignored:
  mov r3, #200
  str r3, [r1, #8]

next_step:
  ldr r4, [r1]    @ r4 = 0x10000 = 65536
  sub r4, r4, #1    @ r4 = 65535
  str r4, [r1]    @ mem[0x10000] = 65535

  ldr r5, [r1]    @ r5 = 65535
  add r6, r5, #1    @ r6 = 65536
  str r6, [r1, #12]   @ mem[0x1000c] = 65536

_stop:
  b _stop