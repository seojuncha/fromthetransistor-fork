@ Test file for ARMv4T assembly directives
@ Filename: test_directives.s

@ Section directives
.section .text
.section .data
.section .bss
.section .rodata

@ Data definition directives
.byte 0x12, 0x34, 0x56, 0x78
.2byte 0x1234, 0x5678
.4byte 0x12345678
.word 0x11223344, 0x55667788
.ascii "Hello World"
.asciz "Null terminated string"
.string "Another null terminated string"
.align 4
.balign 8
.space 100
.skip 50
.zero 64

@ Symbol directives
.global main
.local local_symbol
.extern external_symbol
.weak weak_symbol

@ Constant definitions
.equ CONSTANT1, 0x1000
.set CONSTANT2, 0x2000
.equiv CONSTANT3, 0x3000

@ Macro definitions
.macro PUSH_MULTIPLE reg1, reg2
    stmdb sp!, {\reg1-\reg2}
.endm

.macro POP_MULTIPLE reg1, reg2
    ldmia sp!, {\reg1-\reg2}
.endm

@ Structure directives
.struct 0
user_struct:
    .struct user_struct + 4
name:
    .struct name + 32
age:
    .struct age + 2
height:
    .struct height + 4
.end

@ Start of code section
.section .text
.global _start

_start:
    @ Test using constants
    ldr r0, =CONSTANT1
    ldr r1, =CONSTANT2
    
    @ Test using macros
    PUSH_MULTIPLE r4, r7
    POP_MULTIPLE r4, r7

    @ Test with local labels
1:  
    mov r0, #1
    b 2f
2:
    mov r1, #2
    b 1b

@ Data section with various alignments and data types
.section .data
.align 4

array1:
    .byte 1, 2, 3, 4
    .align 2
array2:
    .2byte 0x1234, 0x5678
    .align 4
array3:
    .4byte 0x12345678
    .align 8
string1:
    .ascii "Test string"
    .align 4

@ BSS section for uninitialized data
.section .bss
    .lcomm buffer1, 1024
    .comm buffer2, 2048
    
.end
