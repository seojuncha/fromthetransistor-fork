# ARM Assembly Related Topics

## System & Python
||Liux|MacOS|Python|
|-|-|-|-|
|Assembler|arm-none-eabi-as|clang|
|Hexdump|hexdump|hexdump|hexdump|
|Disassem|arm-none-eabi-objdump|llvm-objdump|capstone|
|ELF|arm-none-eabi-readelf|llvm-readelf|pyelftools|

## ARM Assembler (Linux)
```shell
$ arm-none-eabi-as --help
$ arm-none-eabi-as -mcpu=arm7tdmi -o ex1.o ex1.s
```
## ARM Assembler (Mac)
```shell
$ brew install llvm
$ clang -target armv4t-none-eabi -c -o ex1.o ex1.s
$ llvm-objdump -d ex1.o
$ llvm-readelf ex1.o
$ llvm-objcopy --input-target=elf32-littlearm --output-target=binary ex1.o ex1.bin
```

## Install Python Tools
```shell
$ pip3 install hexdump
$ pip3 install pyelftools
$ pip3 install capstone
```

## Endianness
