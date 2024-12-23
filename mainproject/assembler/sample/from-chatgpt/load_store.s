.section .text
.global _start

_start:
    LDR r0, =0x1000   @ Load immediate address into r0
    STR r1, [r0]      @ Store r1 at address pointed by r0
    LDR r2, [r0]      @ Load from address pointed by r0 into r2
    STRB r3, [r0]     @ Store byte from r3 into memory
    LDRB r4, [r0]     @ Load byte into r4 from memory
    B .