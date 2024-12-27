@ Test file for ARMv4T instruction set
@ Filename: test_data_processing.s
@ This file covers data processing instructions

.section .text
.global _start

_start:
    @ Test immediate operands
    mov r0, #0
    mov r1, #255
    mov r2, #0xFF00
    movs r3, #1

    @ Test register operands
    mov r4, r0
    movs r5, r1
    mov r6, r2, lsl #4
    mov r7, r3, lsr #1

    @ Test AND operations
    and r0, r1, #0xFF
    ands r0, r1, r2
    and r0, r1, r2, lsl #4
    and r0, r1, r2, lsr #4
    and r0, r1, r2, asr #4
    and r0, r1, r2, ror #4

    @ Test EOR operations
    eor r0, r1, #0xFF
    eors r0, r1, r2
    eor r0, r1, r2, lsl #4
    eor r0, r1, r2, lsr #4
    eor r0, r1, r2, asr #4
    eor r0, r1, r2, ror #4

    @ Test SUB operations
    sub r0, r1, #1
    subs r0, r1, r2
    sub r0, r1, r2, lsl #4
    sub r0, r1, r2, lsr #4
    sub r0, r1, r2, asr #4
    sub r0, r1, r2, ror #4

    @ Test RSB operations
    rsb r0, r1, #1
    rsbs r0, r1, r2
    rsb r0, r1, r2, lsl #4
    rsb r0, r1, r2, lsr #4

    @ Test ADD operations
    add r0, r1, #1
    adds r0, r1, r2
    add r0, r1, r2, lsl #4
    add r0, r1, r2, lsr #4

    @ Test conditional execution
    @ moveq r0, #1
    @ movne r0, #2
    @ movcs r0, #3
    @ movcc r0, #4
    @ movmi r0, #5
    @ movpl r0, #6
    @ movvs r0, #7
    @ movvc r0, #8
    @ movhi r0, #9
    @ movls r0, #10
    @ movge r0, #11
    @ movlt r0, #12
    @ movgt r0, #13
    @ movle r0, #14
    
.end
