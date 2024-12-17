#!/bin/sh

arm-none-eabi-gcc -o uart_test.elf uart_test.c -specs=rdimon.specs -lc -lrdimon -lg

[ $? -eq 0 ] && qemu-system-arm -machine raspi0 -nographic -kernel uart_test.elf -semihosting
