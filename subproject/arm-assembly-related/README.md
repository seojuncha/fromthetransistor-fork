# ARM Assembly Related Topics

## System & Python
||System|Python|
|-|-|-|
|Hexdump|hexdump|hexdump|
|Disassem|arm-none-eabi-objdump|capstone|
|ELF|arm-none-eabi-readelf|pyelftools|

## ARM Assembler
```shell
$ arm-none-eabi-as --help

$ arm-none-eabi-as -mcpu=arm7tdmi -o ex1.o ex1.s
```

## Install Python Tools
```shell
$ pip3 install hexdump
$ pip3 install pyelftools
$ pip3 install capstone
```

## Endianness
