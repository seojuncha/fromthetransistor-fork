OS:=$(shell uname)

ifeq ($(OS), Linux)
	AS = arm-none-eabi-as
	AS_FLAGS = -o
	OBJCOPY = arm-none-eabi-objcopy
	OBJCOPY_FLAGS = -O binary
else ifeq ($(OS), Darwin)
	AS = clang 
	AS_FLAGS = -target armv4t-none-eabi -c -o
	OBJCOPY = llvm-objcopy 
	OBJCOPY_FLAGS = --input-target=elf32-littlearm --output-target=binary
else
	@echo "Unsuported OS"
	exit 1
endif

ex1.bin: ex1.o
	$(OBJCOPY) $(OBJCOPY_FLAGS) ex1.o ex1.bin

ex1.o: ex1.s
	$(AS) $(AS_FLAGS) ex1.o ex1.s

.PHONY: clean

clean:
	rm -rf *.o *.bin