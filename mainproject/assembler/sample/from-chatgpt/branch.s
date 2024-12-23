.section .text
.global _start

_start:
    B label1
    BL label2

label1:
    MOV r0, #1
    B .

label2:
    MOV r1, #2
    B .