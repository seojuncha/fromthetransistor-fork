OS := $(shell uname)

# SAMPLE_DIR1 := sample/from-claude
# SAMPLE_DIR2 := sample/from-chatgpt

SRC_FILES := $(wildcard *.s)
OBJ_FILES := $(patsubst %.s, %.o, $(SRC_FILES))
BIN_FILES := $(patsubst %.s, %.bin, $(SRC_FILES))

# SRC_FILES1 := $(wildcard $(SAMPLE_DIR1)/*.s)
# SRC_FILES2 := $(wildcard $(SAMPLE_DIR2)/*.s)
# OBJ_FILES1 := $(patsubst $(SAMPLE_DIR1)/%.s, $(SAMPLE_DIR1)/%.o, $(SRC_FILES1))
# OBJ_FILES2 := $(patsubst $(SAMPLE_DIR2)/%.s, $(SAMPLE_DIR2)/%.o, $(SRC_FILES2))
# BIN_FILES1 := $(patsubst $(SAMPLE_DIR1)/%.s, $(SAMPLE_DIR1)/%.bin, $(SRC_FILES1))
# BIN_FILES2 := $(patsubst $(SAMPLE_DIR2)/%.s, $(SAMPLE_DIR2)/%.bin, $(SRC_FILES2))

ifeq ($(OS), Linux)
	AS = arm-none-eabi-as
	AS_FLAGS = -march=armv4t -mthumb-interwork -o
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

# all: $(BIN_FILES2) $(OBJ_FILES2) $(BIN_FILES1) $(OBJ_FILES1)
all: $(BIN_FILES) $(OBJ_FILES)

%.bin: %.o
	$(OBJCOPY) $(OBJCOPY_FLAGS) $< $@

%.o: %.s
	$(AS) $(AS_FLAGS) $@ $<

# $(SAMPLE_DIR1)/%.bin: $(SAMPLE_DIR1)/%.o
# 	$(OBJCOPY) $(OBJCOPY_FLAGS) $< $@

# $(SAMPLE_DIR1)/%.o: $(SAMPLE_DIR1)/%.s
# 	$(AS) $(AS_FLAGS) $@ $<

# $(SAMPLE_DIR2)/%.bin: $(SAMPLE_DIR2)/%.o
# 	$(OBJCOPY) $(OBJCOPY_FLAGS) $< $@

# $(SAMPLE_DIR2)/%.o: $(SAMPLE_DIR2)/%.s
# 	$(AS) $(AS_FLAGS) $@ $<

.PHONY: clean
clean:
	rm -rf *.o *.bin
