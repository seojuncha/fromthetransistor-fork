# Implement ARM CPU in Verilog

## CPU Core

### Registers
General Registers

R0~R15

word(32-bit) size


Current Program Status Register (CPSR)

```
+-31-+-30-+-29-+-28-+-27-+-26-----------8-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| N  | Z  | C  | V  | Q  |      DNM       | I | F | T | M | M | M | M | M |
+----+----+----+----+----+----------------+---+---+---+---+---+---+---+---+
|            condition                    |             control           |
N (Negative)
Z (Zero)
C (Carry)
V (oVerflow)
T (Thumb) : should be zero
M[4:0] (Mode) : should be zero(user mode)
```

### Pipeline
Fetch

1. Load an instruction from the memory
2. Increment the program counter(?)

Decode


Execute


### Interfaces
Control Interface
- clock
- reset

Memory Bus Interface
- address
- data in
- data out


## Memory
BRAM




32-bit address: 2^32 = 4,294,967,296 (4GB)<br>
Available address space : 4GB

```
+-------------+ 0xff... (MSB)
| 32-bit word |
+-------------+
| 32-bit word |
+-------------+
|    ....     |
+-------------+
| 32-bit word |
+-------------+
| 32-bit word |
+-------------+ 0x00... (LSB)
```
So what is the number of word when I define a mmemory size.

total memory size = word * #word so,<br>
#word = total memory size / word

For example,
```
1) memory size = 32KB
#word = 32KB(bytes) / 4B(ytes)
      = 8K = 8*1024
      = 8,192

2) memory size = 64KB
#word = 64K / 4
      = 16K = 16*1024
      = 16,384

3) memory size = 1MB
#word = 1M / 4
      = 1024K / 4
      = 256K = 256*1024
      = 262,144
```

Memory Map
```
BRAM
size: 32K
start: 0x0000_0000
end:   0x0000_1FFF
```
```
SRAM
size: 32K
start: 0x0000_2000
end:   0x0000_3FFF
```
```
MMIO
size: 4K
start: 0x0000_4000
end: : 0x0000_4FFF
```
### Usage of $readmemb
```shell
# convert raw binary data to text-based binary representation.
$ xxd -b -c 4 data_processing.bin | awk '{print $2 $3 $4 $5}' > data_processing_txt.bin
```


## cocotb execute
```shell
COCOTB_RESOLVE_X=ZEROS make
```


## Shifter
For data processing instruction, there are three types.

Control Signal & Data Path

Control Signal  
is shifted by 32-bit immediate value?  
is shifted by only register value?  
is shifted by immediate value?  
is shifted by register value?  
shift type
: 2 bit

```
Data Path  
shift amount size  
is shifted by 32-bit immediate value?
: 4 bit (rotate_imm)
is shifted by only register value?
: none
is shifted by immediate value?
: 5 bit (shift_imm)
is shifted by register value?
: 8 bit (rs[7:0])

shifted value size  
is shifted by 32-bit immediate value?  
: 8 bit (imm8)
is shifted by only register value?  
: 32 bit (rm)
is shifted by immediate value?  
: 32 bit (rm)
is shifted by register value?  
: 32 bit (rm)
```

32-bit immediate  
```
bit[11:8] = rotate_imm
bit[7:0] = imm_8
```
Note, only bit[25] set to 1.

direct reg
```
bit[11:4] = 0
bit[3:0] = rm
```

immediate shift  
```
bit[11:7] = shift_imm
bit[6:5] = shift
bit[4] = 0
bit[3:0] = rm
```

register shift  
```
bit[11:8] = rs
bit[7] = 0
bit[6:5] = shift
bit[4] = 1
bit[3:0] = rm
```
shift value = rs[7:0]




### Shifter Carry Out
Examples,<br>
```python
>>> bin(0b1100 << 1)
0b11000  # out of range of 4-bit, carry out!
>>> bin(0b0110 << 1)
0b1100   # keep 4-bit, not carry out.
```


## ALU