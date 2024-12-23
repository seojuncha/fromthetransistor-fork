@ Test file for complex ARMv4T instruction combinations
@ Filename: test_complex.s

.section .text
.global _start

_start:
    @ Complex addressing modes with shifting
    ldr r0, [r1, r2, lsl #2]!
    str r0, [r3, r4, lsr #1]
    ldr r5, [r6, r7, asr #3]
    str r8, [r9, r10, ror #4]

    @ Multiple operations with condition codes
    addeq r0, r1, r2, lsl #2
    subne r3, r4, r5, lsr #1
    rsbcs r6, r7, r8, asr #3
    addcc r9, r10, r11, ror #4

    @ Complex load/store multiple with write-back
    ldmdb r0!, {r1, r2, r3, r4, r6, r8, r9, r10}
    stmia r1!, {r2, r3, r4, r5, r7, r9, r10, r11}

    @ Nested conditional execution
    moveq r0, #1
    addeq r1, r2, #2
    subeq r3, r4, r5
    streq r6, [r7, #4]!

    @ Complex arithmetic combinations
    sub r0, r1, r2, lsl r3
    add r4, r5, r6, lsr r7
    rsb r8, r9, r10, asr r11
    add r12, r14, r0, ror r1

    @ Mixed load/store with arithmetic
    ldr r0, [r1, #4]
    add r0, r0, #1
    str r0, [r1, #4]!
    sub r1, r1, #4

    @ Register-based shifts with conditions
    mov r0, r1, lsl r2
    movne r3, r4, lsr r5
    moveq r6, r7, asr r8
    movcs r9, r10, ror r11

    @ Complex flag-setting operations
    cmp r0, r1, lsl #2
    teq r2, r3, lsr #3
    tst r4, r5, asr #4
    cmn r6, r7, ror #5

    @ Multiple operations sequence
complex_sequence:
    push {r4-r11, lr}
    mov r4, #0              @ Initialize counter
    ldr r5, [r0, #0]        @ Load base address
    add r6, r5, #1024       @ Calculate end address

1:  @ Local label for loop
    ldr r7, [r5], #4        @ Load word and update pointer
    add r8, r7, r4          @ Add counter
    mul r9, r8, r7          @ Multiply
    str r9, [r6], #4        @ Store result and update pointer
    add r4, r4, #1          @ Increment counter
    cmp r4, #256           @ Check loop condition
    bne 1b                  @ Branch if not done

    @ Memory copy with alignment checks
memory_copy:
    push {r4-r9}
    mov r2, r2, lsr #2      @ Convert size to words
    
1:  @ Copy loop
    ldmia r0!, {r4-r7}      @ Load 4 words
    stmia r1!, {r4-r7}      @ Store 4 words
    subs r2, r2, #4         @ Decrement counter
    bgt 1b                  @ Continue if not done
    
    pop {r4-r9}

    @ Complex bit manipulation
bit_ops:
    ldr r0, [r1]            @ Load original value
    and r2, r0, #0xFF       @ Mask lower byte
    mov r3, r0, lsr #8      @ Shift right 8 bits
    orr r4, r2, r3, lsl #16 @ Combine with shift
    bic r5, r4, #0x0F00     @ Clear bits 8-11
    eor r6, r5, r5, ror #4  @ XOR with rotated value

    @ Stack manipulation with multiple registers
stack_ops:
    push {r4-r11}           @ Save registers
    mov r4, sp              @ Save stack pointer
    sub sp, sp, #16         @ Allocate stack space
    
    @ Do some stack operations
    str r0, [sp, #0]
    str r1, [sp, #4]
    str r2, [sp, #8]
    str r3, [sp, #12]
    
    @ Restore stack and registers
    mov sp, r4
    pop {r4-r11}

    @ Exit sequence
exit:
    mov r0, #0
    mov r7, #1              @ syscall number for exit
    swi 0x0                 @ software interrupt

.end
