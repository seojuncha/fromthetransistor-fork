@ Test file for ARMv4T instruction set
@ Filename: test_branch.s
@ This file covers branch operations

.section .text
.global _start

_start:
    @ Test basic branches
    b label1
    bl label2
    bx lr
    
label1:
    @ Test conditional branches
    beq label3
    bne label3
    bcs label3
    bcc label3
    bmi label3
    bpl label3
    bvs label3
    bvc label3
    bhi label3
    bls label3
    bge label3
    blt label3
    bgt label3
    ble label3
    
label2:
    @ Test branch with link conditional
    bleq label3
    blne label3
    blcs label3
    blcc label3
    blmi label3
    blpl label3
    blvs label3
    blvc label3
    blhi label3
    blls label3
    blge label3
    bllt label3
    blgt label3
    blle label3
    
label3:
    @ Test branch and exchange
    bx r0
    bx r1
    bx r2
    bx r3
    bx r4
    bx r5
    bx r6
    bx r7
    bx r8
    bx r9
    bx r10
    bx r11
    bx r12
    bx r14

.end
