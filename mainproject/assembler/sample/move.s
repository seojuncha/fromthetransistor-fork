_start:

@ -----------------------------------------------------------------------------
@ 20 x MOV instructions
@ -----------------------------------------------------------------------------

@ --- Immediate loads (if encodable by assembler) ---
MOV     r0,  #0x0          @ Load 0 into r0
MOV     r1,  #0x1          @ Load 1 into r1
MOV     r2,  #0xFF         @ Load 0xFF (255) into r2
@MOV     r3,  #0x1234       @ Load 0x1234 into r3 (assembler may use a literal pool if needed)
@MOV     r4,  #0xABCD       @ Another example immediate load

@ --- MOV with immediate shifts ---
MOV     r5,  r4, LSL #4    @ r5 = r4 << 4
MOV     r6,  r5, LSR #2    @ r6 = r5 >> 2 (logical)
MOV     r7,  r6, ASR #3    @ r7 = r6 >> 3 (arithmetic)
MOV     r8,  r7, ROR #8    @ r8 = rotate r7 right by 8 bits
MOV     r9,  r8, RRX       @ r9 = r8 rotate right by 1 with extend (ROR #0)

@ --- MOV with register-based shifts ---
MOV     r10, r1, LSL r2    @ r10 = r1 << r2
MOV     r11, r10, LSR r3   @ r11 = r10 >> r3 (logical)
MOV     r12, r11, ASR r4   @ r12 = r11 >> r4 (arithmetic)
MOV     r13, r12, ROR r5   @ r13 = rotate r12 right by r5
MOV     r14, r13, RRX      @ r14 = rotate r13 right by 1 (carry in)

@ --- More immediate experiments ---
@MOV     r0,  #0xAABB       @ Load 0xAABB
MOV     r1,  #0xCC         @ Load 0xCC
MOV     r2,  r0,  ROR #8   @ r2 = rotate 0xAABB by 8 bits
MOV     r3,  r1,  RRX      @ r3 = rotate 0xCC right by 1
MOV     r4,  pc           @ Example: store current PC in r4

@ -----------------------------------------------------------------------------
@ 20 x MVN instructions
@ -----------------------------------------------------------------------------

@ --- MVN immediate loads (NOT of imm) ---
MVN     r5,  #0x0          @ r5 = NOT(0x00000000) = 0xFFFFFFFF
MVN     r6,  #0xFF         @ r6 = NOT(0x000000FF) = 0xFFFFFF00
@MVN     r7,  #0x1234       @ r7 = NOT(0x00001234)
@MVN     r8,  #0xABCD       @ r8 = NOT(0x0000ABCD)
MVN     r9,  #0x1          @ r9 = NOT(0x00000001) = 0xFFFFFFFE

@ --- MVN with immediate shifts ---
MVN     r10, r9, LSL #3    @ r10 = NOT(r9 << 3)
MVN     r11, r10, LSR #2   @ r11 = NOT(r10 >> 2) (logical)
MVN     r12, r11, ASR #1   @ r12 = NOT(r11 >> 1) (arithmetic)
MVN     r13, r12, ROR #4   @ r13 = NOT(rotate r12 right by 4)
MVN     r14, r13, RRX      @ r14 = NOT(rotate r13 right by 1 with extend)

@ --- MVN with register-based shifts ---
MVN     r0,  r0,  LSL r5   @ r0 = NOT(r0 << r5)
MVN     r1,  r1,  LSR r6   @ r1 = NOT(r1 >> r6) (logical)
MVN     r2,  r2,  ASR r7   @ r2 = NOT(r2 >> r7) (arithmetic)
MVN     r3,  r3,  ROR r8   @ r3 = NOT(rotate r3 right by r8)
MVN     r4,  r4,  RRX      @ r4 = NOT(r4 rotate right by 1)

@ --- More immediate experiments ---
@MVN     r5,  #0xAABB       @ r5 = NOT(0xAABB)
MVN     r6,  #0xCC         @ r6 = NOT(0xCC)
MVN     r7,  r5,  ROR #8   @ r7 = NOT(r5 rotate right by 8)
MVN     r8,  r6,  RRX      @ r8 = NOT(r6 rotate right by 1)
MOV     pc,  lr           @ Return / end

.end
