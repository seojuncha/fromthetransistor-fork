#!/bin/sh

arm-none-eabi-gcc -o semihosting_test.elf semihosting_test.c -specs=rdimon.specs -lc -lrdimon -lg

# WARN: Do not set machine as virt only. If that, semihosting isn't working.
[ $? -eq 0 ] && qemu-system-arm -machine raspi0 -nographic -kernel semihosting_test.elf -semihosting --trace "*" -D trace.log
