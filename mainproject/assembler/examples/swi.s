.section .text
.global _start

_start:
    SWI 0x123456
    B .
