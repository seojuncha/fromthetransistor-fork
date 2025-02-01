.section .text
.global _start

 @ 0x0
_start:
  mov r0, #10    @ r0 = 10
  mov r1, #20    @ r1 = 20

  add r2, r0, r1    @ r2 = 10 + 20 = 30
  sub r3, r2, #5    @ r3 = 30 + 5 = 35

  mov r4, #0x00010000    @ r4 = 0x10000
  str r3, [r4]    @ mem[0x10000] <- 35
  ldr r5, [r4]    @ r5 = 35

  add r5, r5, #7    @ r5 = 42
  str r5, [r4, #4]    @ mem[0x10004] <- 42
  ldr r6, [r4, #4]    @ r6 = 42

  mov r7, #5    @ r7 = 5

@ 0x2c
loop:
  sub r7, r7, #1
  cmp r7, #0
  b loop_check

@ 0x38
loop_check:
  mov r8, r7
  add r8, r8, #1
  sub r9, r8, #6
  b end_loop

@ 0x48
end_loop:
_stop:
  b _stop
