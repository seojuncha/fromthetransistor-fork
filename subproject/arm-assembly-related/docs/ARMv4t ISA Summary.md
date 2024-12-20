# ARMv4T Instruction Set Summary

## Overview
The ARMv4T architecture supports both 32-bit ARM and 16-bit Thumb instruction sets, providing high performance and compact code. Below is a summary of key features, including instruction encoding, bit representation, and byte-level details.

---

## 1. Instruction Set Basics

### 1.1 ARM Instruction Set
- **Instruction Length**: 32 bits.
- **Addressing Modes**: Immediate, Register, Scaled Register, and PC-relative.
- **Conditional Execution**: Most instructions are conditionally executed based on the status flags.

### 1.2 Thumb Instruction Set
- **Instruction Length**: 16 bits.
- **Purpose**: Optimized for code density in memory-constrained systems.
- **Features**: Subset of ARM instructions, converted dynamically for execution.

---

## 2. Instruction Encoding

### 2.1 ARM Instruction Format (32 bits)
| Field          | Bits   | Description                                       |
|----------------|--------|---------------------------------------------------|
| Condition      | [31:28]| Specifies the condition for execution.           |
| Opcode         | [27:21]| Operation code (e.g., ADD, SUB).                 |
| S-bit          | [20]   | Updates condition flags if set.                  |
| Operand1       | [19:16]| First operand register.                          |
| Operand2       | [15:0] | Second operand (register or immediate value).     |

### 2.2 Thumb Instruction Format (16 bits)
| Field          | Bits   | Description                                       |
|----------------|--------|---------------------------------------------------|
| Opcode         | [15:11]| Operation code.                                  |
| Register1      | [10:8] | First operand register.                          |
| Immediate/Reg2 | [7:0]  | Second operand (register or immediate value).     |

---

## 3. Conditional Execution (ARM Only)
ARM uses a 4-bit condition field to specify execution conditions based on flags in the CPSR register:

| Condition Code | Binary | Meaning          | Flags Checked                  |
|----------------|--------|------------------|---------------------------------|
| EQ             | 0000   | Equal            | Z = 1                          |
| NE             | 0001   | Not Equal        | Z = 0                          |
| GT             | 1100   | Greater Than     | Z = 0, N = V                   |
| LT             | 1011   | Less Than        | N != V                         |
| AL             | 1110   | Always           | No condition                   |

---

## 4. Data Processing Instructions
```
31 30 29 28 27 26 25 24 23 22 21 20 19 16 15 12 11  0
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|  Cond  |00|I| OpCode |S|   Rn  |  Rd  |   Operand2 |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
```
### 4.1 Common Operations
| Mnemonic | Operation        | Syntax              | Example                  |
|----------|------------------|---------------------|--------------------------|
| ADD      | Add              | `ADD Rd, Rn, Rm`   | `ADD R0, R1, R2`         |
| SUB      | Subtract         | `SUB Rd, Rn, Rm`   | `SUB R3, R4, #5`         |
| MOV      | Move             | `MOV Rd, Operand2` | `MOV R5, #0x1F`          |

### 4.2 Bit Encoding for Data Processing
| Field          | Bits   | Description                           |
|----------------|--------|---------------------------------------|
| Condition      | [31:28]| Execution condition.                 |
| Opcode         | [24:21]| Operation code (e.g., 0100 for ADD).  |
| Immediate Flag | [25]   | 1 if Operand2 is immediate.           |
| Destination    | [15:12]| Destination register (Rd).            |
| Source         | [19:16]| First operand register (Rn).          |
| Operand2       | [11:0] | Second operand.                       |

---

## 5. Load/Store Instructions
```
31 30 29 28 27 26 25 24 23 22 21 20 19 16 15 12 11  0
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|  Cond  |01|I|P|U|B|W|L|   Rn  |  Rd  |   Offset   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
```

### 5.1 Common Load/Store Instructions
| Mnemonic | Operation               | Syntax               | Example                 |
|----------|-------------------------|----------------------|-------------------------|
| LDR      | Load Register           | `LDR Rd, [Rn, #imm]`| `LDR R0, [R1, #4]`      |
| STR      | Store Register          | `STR Rd, [Rn, #imm]`| `STR R2, [R3, #8]`      |
| LDM      | Load Multiple Registers | `LDM Rn, {Rlist}`   | `LDM R4, {R1, R2, R3}`  |
| STM      | Store Multiple Registers| `STM Rn, {Rlist}`   | `STM R5, {R6, R7}`      |

### 5.2 Bit Encoding for Load/Store
| Field          | Bits   | Description                           |
|----------------|--------|---------------------------------------|
| Condition      | [31:28]| Execution condition.                 |
| Opcode         | [27:20]| Operation code.                      |
| Immediate Flag | [25]   | 1 if offset is immediate.             |
| Base Register  | [19:16]| Base address register (Rn).           |
| Destination    | [15:12]| Destination/source register (Rd).     |
| Offset         | [11:0] | Address offset or immediate value.    |

---

## 6. Branch Instructions
```
31 30 29 28 27 26 25 24 23                        0
+--+--+--+--+--+--+--+--+------------------------+
|  Cond  |101| L |        24-bit offset          |
+--+--+--+--+--+--+--+--+------------------------+
```

### 6.1 Unconditional Branch
| Mnemonic | Syntax       | Description          |
|----------|--------------|----------------------|
| B        | `B label`    | Branch to label.     |
| BL       | `BL label`   | Branch and link.     |

### 6.2 Conditional Branch (ARM)
| Mnemonic | Syntax         | Condition Example |
|----------|----------------|-------------------|
| BEQ      | `B label, EQ`  | Z = 1             |
| BNE      | `B label, NE`  | Z = 0             |

### 6.3 Bit Encoding for Branch
| Field       | Bits   | Description                           |
|-------------|--------|---------------------------------------|
| Condition   | [31:28]| Execution condition.                 |
| Link Flag   | [24]   | 1 for branch and link.               |
| Offset      | [23:0] | Signed offset to branch target.      |

---

## 7. Byte and Endianness Considerations
ARM supports both little-endian and big-endian byte order. Instructions and data are affected by endianness, configurable via the CPSR register.

- **Little-Endian**: Least significant byte is stored at the lowest memory address.
- **Big-Endian**: Most significant byte is stored at the lowest memory address.

Example:
- Value: `0x12345678`
  - Little-endian memory: `78 56 34 12`
  - Big-endian memory: `12 34 56 78`

---

## 8. Summary of Key Features
- **Bitwise Operations**: AND, ORR, EOR, and BIC.
- **Shift Operations**: Logical shift left/right, arithmetic shift right, and rotate.
- **Condition Codes**: Execute instructions based on CPSR flags.
- **Compact Thumb Instructions**: Useful for reducing code size in embedded systems.
- **Flexibility in Addressing Modes**: Scaled, immediate, and PC-relative options.
