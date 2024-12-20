AS=arm-none-eabi-as
OBJCOPY=arm-none-eabi-objcopy

ex1.bin: ex1.o
	$(OBJCOPY) -O binary ex1.o ex1.bin

ex1.o: ex1.s
	$(AS) -o ex1.o ex1.s

.PHONY: clean

clean:
	rm -rf *.o *.bin