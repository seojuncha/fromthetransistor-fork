# Memory

# Memory Map
1. define address range
2. implement address decoding
3. pass address to memory modules

## Define Address Range
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
32KB = 2^5 * 2^10 = 2^15 Bytes
#word = 32KB(bytes) / 4B(ytes)
      = 8K = 8*1024
      = 8,192

2) memory size = 64KB
64KB = 2^6 * 2^10 = 2^16 Bytes
#word = 64K / 4
      = 16K = 16*1024 = 2^4
      = 16,384

3) memory size = 1MB
#word = 1M / 4
      = 1024K / 4
      = 256K = 256*1024
      = 262,144
```

### End address calculation
If memory size is 64KB, 2^16 Bytes.
End address = start address + 64K - 1
= start address + 65535
= start address + 0xffff

So, Memory Map is
```
Flash
size: 4K
start: 0x0000_0000 - 0x0000_0FFF
```

```
BRAM
size: 64K
range: 0x0001_0000 - 0x0000_FFFF
```

```
SRAM
size: 64K
start: 0x0002_0000 - 0x0001_FFFF
```
```
MMIO
size: 4K
start: 0x0003_0000 - 0x0003_0FFF
```

## Address Decoding


# Types of Memory

## BRAM
## SRAM
## DRAM
## Flash
## ROM