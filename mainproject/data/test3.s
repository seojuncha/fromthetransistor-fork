.section .text
.global _start

_start:
  mov r0, #10      @ r0 = 10
  mov r1, #20      @ r1 = 20
  add r2, r0, r1   @ r2 = 30
  sub r3, r2, #5   @ r3 = 25

  ldr r4, =data
  ldr r5, [r4]
  str r5, [r4, #4]

  B end

loop:
  B loop

end:
  mov r7, #1
  mov r0, #0
  
  .data
data:
  .word 100
  .word 0
