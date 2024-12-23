.section .text
.global _start

_start:
    LSL r0, r1, #2    @ Logical shift left
    LSR r2, r1, #2    @ Logical shift right
    ASR r3, r1, #2    @ Arithmetic shift right
    ROR r4, r1, #2    @ Rotate right
    B .