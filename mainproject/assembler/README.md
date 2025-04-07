## assembler

This module converts `.s` assembly files into binary files that can be loaded into memory or executed by the MCU.

## Requirements
- Python3
- ARM toolchains

```shell
# For Linux
$ ./install-toolchain.sh
```

## Usage

Supported command line options
```shell
$ ./as.py --help
usage: as [--debug] [--verify] files...

positional arguments:
  files       Input assembly files

options:
  -h, --help  show this help message and exit
  --debug     Print debug messages
  --verify    Compare with the original ARM assembler
```

### Basic Usage
```shell
$ ./as.py examples/move.s
```

## Directory Structure
```
assembler/
├── as.py                  # Main assembly logic
├── as_encoder.py          # Encodes instructions to binary code
├── as_parser.py           # Asembly code parsing logic
├── as_obj.py              # Classes for instruction categories
├── instruction_set.py     # Instruction definitions (mnemonics, opcodes)
├── install-toolchain.py   # ARM toolchain install script (linux)
├── examples/              # Example .s files
└── output/                # Generated binary or text output
```
## Design Philosophy

The goal of this assembler is to:
- Focus on **simplicit**y and **clarit**y, not performance.
- Be written in **Python** for easy experimentation and readability.
- Follow **ARMv4 instruction set** except for Thumb mode and debug features.

## Implementation Strategy: Two-Pass
This assembler uses a **two-pass strategy**:

1. **First Pass** – Scan the entire source code to build a symbol table with all label addresses.
2. **Second Pass** – Re-parse the code, replacing labels with addresses and emitting binary instructions.

This approach makes it easier to handle forward-references and provides more predictable behavior.

### Instructions
Based on ARMv4 ISA(Instruction Set Architecture)

Intetionally Missing Features
- Debug related
- Thumb mode
- User mode

### Supported Instructions
- Data Processing
  - `add`, `sub`, `mov`, `mvn`, `mul`, `and`, `orr`, `eor`, `cmp`, `teq`, `tst`
- Memory Access
  - `ldr`, `str`
- Branch
  - `b`

## Testing Strategy
We use binary-level comparison to validate our assembler against an industry-standard ARM toolchain.

### Automated Verification Process
When the `--verify` flag is enabled, the assembler performs the following steps:

1. **Assemble the input file using our custom assembler**, generating a `.bin` binary file.

2. **Assemble the same file using the official ARM toolchain**:
- On Linux: `arm-none-eabi-as` + `arm-none-eabi-objcopy`
- On macOS: `clang` + `llvm-objcopy`

3. **Compare the two binary files** byte-by-byte.

4. If the outputs match, print Verified!!. Otherwise, a cmp diff is shown.

### Example
```shell
$ ./as.py --verify examples/move.s
```
If verification fails, the tool provides a byte-by-byte difference internally:
```shell
$ cmp -l examples/swi.bin output/swi.bin
```

> Note: Writes binary in little-endian, 32-bit aligned format.

## Debug Mode
When the `--debug` flag is enabled, the assembler provides detailed output about the parsing and encoding process.

### What the Debug Output Shows
The debug output includes multiple stages:
### 1. Raw Tokens (Lexing Stage)
```
LINE: ['MOV', 'r0,', '#1']
LINE: ['MOV', 'r0,', '#2']
LINE: ['B', '.']
...
```
Each line of assembly is tokenized and printed as a list of strings. This helps verify the correctness of instruction parsing.

### 2. Parsed Instruction Structure
After parsing, each instruction is displayed as a structured object. For example:
```
[0x0]       MOV r0, #1
rd      [R0]     rn     [R0]
imm     [#1]     rm     [R0]
shifter [None]   shift imm [#None] rs [RNone]
```
Each operand is broken down and labeled, showing:
- Which registers are used (rd, rn, rm)
- Immediate values (imm)
- Shifting behavior (if any)

This structure reveals how the parser internally understands each instruction.

### 3. Branch Instructions with Target Resolution
```
[0x8]       B .
target addr [0x8]

[0xc]       B label1
target addr [0x0]

[0x10]      BL label2
target addr [0x14]
```
Branch instructions display their target address clearly. If a label is used, its resolved address is shown after label resolution (second pass).

### 4. Final Binary Emission
At the bottom, the final encoded 32-bit instructions are printed alongside their addresses:
```
[0x0]  e3a00001
[0x4]  e3a00002
[0x8]  eafffffe
[0xc]  eafffffb
[0x10] ebfffffe
[0x14] e3a01002
[0x18] eafffffe
```
Each instruction is:
- 32 bits
- Printed in hex format
- Aligned to 4-byte addresses
- Emitted in little-endian format to the .bin file

## Planned Features
-  Support more instructions
    - Other branch instructions, e.g. `bx`,`bl`
- Support lables
- Error handling
