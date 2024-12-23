OS := $(shell uname)

# SAMPLE_DIR := sample/from-claude
SAMPLE_DIR := sample/from-chatgpt
OBJ_DIR := $(SAMPLE_DIR)/objdir

SRC_FILES := $(wildcard $(SAMPLE_DIR)/*.s)
OBJ_FILES := $(patsubst $(SAMPLE_DIR)/%.s, $(OBJ_DIR)/%.o, $(SRC_FILES))
BIN_FILES := $(patsubst $(SAMPLE_DIR)/%.s, $(OBJ_DIR)/%.bin, $(SRC_FILES))

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

all: $(BIN_FILES) $(OBJ_FILES)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

$(OBJ_DIR)/%.bin: $(OBJ_DIR)/%.o | $(OBJ_DIR)
	$(OBJCOPY) $(OBJCOPY_FLAGS) $< $@

$(OBJ_DIR)/%.o: $(SAMPLE_DIR)/%.s | $(OBJ_DIR)
	$(AS) $(AS_FLAGS) $@ $<

.PHONY: clean
clean:
	rm -rf $(OBJ_DIR)/*.o $(OBJ_DIR)/*.bin
	rm -rf $(OBJ_DIR)