#!/bin/sh

arm-none-eabi-gcc -o uart_main.elf main.c uart.c -specs=rdimon.specs -lc -lrdimon -lg

# WARN: Do not all pl011 trace events set, it makes too large log file

# [ $? -eq 0 ] && qemu-system-arm -machine versatilepb -nographic -kernel uart_main.elf -semihosting -serial mon:stdio --trace "pl011_read_*" --trace "pl011_write_*" -D trace.log
# [ $? -eq 0 ] && qemu-system-arm -machine raspi0 -nographic -kernel uart_main.elf -semihosting --trace "pl011_read_*" --trace "pl011_write" --trace "pl011_fifo*"

# Let's use virtual machine to insert verilog cpu later.
[ $? -eq 0 ] && qemu-system-arm -machine virt -cpu cortex-a7 -nographic -kernel uart_main.elf -semihosting --trace "pl011_read_*" --trace "pl011_write" --trace "pl011_fifo*"