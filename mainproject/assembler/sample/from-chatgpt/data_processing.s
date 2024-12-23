.section .text
.global _start

_start:
    MOV r0, #1        @ Move immediate value 1 to r0
    ADD r1, r0, #2    @ r1 = r0 + 2
    SUB r2, r1, #1    @ r2 = r1 - 1
    MUL r3, r1, r2    @ r3 = r1 * r2
    AND r4, r0, r1    @ r4 = r0 AND r1
    ORR r5, r0, r1    @ r5 = r0 OR r1
    EOR r6, r0, r1    @ r6 = r0 XOR r1
    MVN r7, r0        @ r7 = NOT r0
    CMP r0, r1        @ Compare r0 and r1
    TEQ r0, r1        @ Test for equality
    TST r0, r1        @ Test bits
    B .              @ Infinite loop