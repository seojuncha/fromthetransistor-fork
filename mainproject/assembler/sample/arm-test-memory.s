@ Test file for ARMv4T instruction set
@ Filename: test_memory_ops.s
@ This file covers memory operations

.section .text
.global _start

_start:
    @ Test basic load/store
    ldr r0, [r1]
    ldr r0, [r1, #4]
    ldr r0, [r1, #-4]
    ldr r0, [r1, r2]
    ldr r0, [r1, r2, lsl #2]
    
    @ Test pre-indexed addressing
    ldr r0, [r1, #4]!
    ldr r0, [r1, -r2]!
    ldr r0, [r1, r2, lsl #2]!
    
    @ Test post-indexed addressing
    ldr r0, [r1], #4
    ldr r0, [r1], -r2
    ldr r0, [r1], r2, lsl #2
    
    @ Test store operations
    str r0, [r1]
    str r0, [r1, #4]
    str r0, [r1, #-4]
    str r0, [r1, r2]
    str r0, [r1, r2, lsl #2]
    
    @ Test byte load/store
    ldrb r0, [r1]
    ldrb r0, [r1, #4]
    strb r0, [r1]
    strb r0, [r1, #4]
    
    @ Test halfword load/store
    ldrh r0, [r1]
    ldrh r0, [r1, #4]
    strh r0, [r1]
    strh r0, [r1, #4]
    
    @ Test signed halfword/byte load
    ldrsb r0, [r1]
    ldrsb r0, [r1, #4]
    ldrsh r0, [r1]
    ldrsh r0, [r1, #4]
    
    @ Test multiple load/store (avoiding base register in list when using writeback)
    ldmia r0!, {r1-r4}         @ OK: r0 not in list
    ldmib r0!, {r1-r4}         @ OK: r0 not in list
    ldmda r0!, {r1-r4}         @ OK: r0 not in list
    ldmdb r0!, {r1-r4}         @ OK: r0 not in list
    
    @ Store multiple (avoiding base register in list when using writeback)
    stmia r0!, {r1-r4}         @ OK: r0 not in list
    stmib r0!, {r1-r4}         @ OK: r0 not in list
    stmda r0!, {r1-r4}         @ OK: r0 not in list
    stmdb r0!, {r1-r4}         @ OK: r0 not in list
    
    @ Multiple load/store without writeback (can include base register)
    ldmia r0, {r0-r3}          @ OK: no writeback
    stmia r0, {r0-r3}          @ OK: no writeback

    @ Test load/store with write-back (using different base register)
    ldmia r0!, {r1-r4}         @ Use r0 as base, load into different registers
    stmia r1!, {r2-r5}         @ Use r1 as base, store from different registers

.end
