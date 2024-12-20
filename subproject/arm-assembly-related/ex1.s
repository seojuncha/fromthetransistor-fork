.section .text
.global _start

_start:
  MOV R0, #5
  MOV R1, #10
  ADD R2, R0, R1
  B _start
