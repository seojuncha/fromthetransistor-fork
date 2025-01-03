.section .text
.global _start

_start:
    MOVEQ r0, #0      @ Move if equal
    MOVNE r1, #1      @ Move if not equal
    MOVLT r2, #2      @ Move if less than
    MOVGT r3, #3      @ Move if greater than
    B .
