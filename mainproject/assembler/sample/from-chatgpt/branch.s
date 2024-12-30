.section .text
.global _start

label1:
    MOV r0, #1
    MOV r0, #2
    B .

_start:
    B label1
    BL label2

label2:
    MOV r1, #2
    B .
