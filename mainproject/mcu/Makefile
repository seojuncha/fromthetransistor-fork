SIM = icarus
TOPLEVEL_LANG = verilog
WAVES = 1
TOPLEVEL = mcu
MODULE = tb_mcu
VERILOG_SOURCES += \
mcu.v \
cpu/verilog/cpu.v \
cpu/verilog/register_files.v \
cpu/verilog/instruction_decoder.v \
cpu/verilog/alu.v \
cpu/verilog/barrel_shifter.v \
memory/verilog/memory_controller.v \
memory/verilog/address_decoder.v \
memory/verilog/bram.v \
memory/verilog/sram.v \
memory/verilog/flash.v \
memory/verilog/peripheral.v

include $(shell cocotb-config --makefiles)/Makefile.sim