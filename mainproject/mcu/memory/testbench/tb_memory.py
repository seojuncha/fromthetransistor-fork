import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

BRAM_BASE_ADDRESS = 0x0000_0000
SRAM_BASE_ADDRESS = 0x0001_0000
FLASH_BASE_ADDRESS = 0x0002_0000
PERIPHERAL_BASE_ADDRESS = 0X0003_0000
ADDRESS_OFFSET_MAX = 0xffff

def has_error(dut):
  if dut.decode_error.value:
    return "DECODE ERROR"
  if dut.flash_error.value:
    return "FLASH ERROR"
  if dut.error.value:
    return "CONTROLLER ERROR"
  return "SUCCESS"

def selected_memory_type(dut):
  if dut.bram_selected.value == 1: mem_type = "bram"
  elif dut.sram_selected.value == 1: mem_type = "sram"
  elif dut.flash_selected.value == 1: mem_type = "flash"
  elif dut.peripheral_selected.value == 1: mem_type = "peripheral"
  else: mem_type = "unknown"
  return mem_type

async def test_address_decoding(dut):
  dut.addr.value = BRAM_BASE_ADDRESS
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = BRAM_BASE_ADDRESS + ADDRESS_OFFSET_MAX
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = SRAM_BASE_ADDRESS
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = SRAM_BASE_ADDRESS + ADDRESS_OFFSET_MAX
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = FLASH_BASE_ADDRESS
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = FLASH_BASE_ADDRESS + ADDRESS_OFFSET_MAX
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = PERIPHERAL_BASE_ADDRESS
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = PERIPHERAL_BASE_ADDRESS + ADDRESS_OFFSET_MAX
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = dut.addr.value + 1
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")


@cocotb.test()
async def tb_memory(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await Timer(10, units="ns")

  # wait for reset
  dut.rst.value = 1
  await RisingEdge(dut.clk)

  # Initial state: should be "error" with the "unkown" memory type.
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  await test_address_decoding(dut)
  await RisingEdge(dut.clk)

  dut.cpu_write_mem.value = 1
  dut.addr.value = BRAM_BASE_ADDRESS
  dut.idata_from_cpu.value = 2**32 - 1
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} <- 0x{dut.bram_mem.idata.value.integer:08x}")


  dut.addr.value = SRAM_BASE_ADDRESS
  dut.idata_from_cpu.value = (2**32 - 1) >> 16
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} <- 0x{dut.sram_mem.idata.value.integer:08x}")
  await Timer(1, units="ns")

  dut.cpu_read_mem.value = 1
  dut.cpu_write_mem.value = 0
  dut.addr.value = BRAM_BASE_ADDRESS
  await RisingEdge(dut.clk)
  await Timer(1, units="ns")
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.bram_odata.value.integer:08x}")

  dut.addr.value = SRAM_BASE_ADDRESS
  await RisingEdge(dut.clk)
  await Timer(1, units="ns")
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.sram_odata.value.integer:08x}")

  dut.addr.value = SRAM_BASE_ADDRESS + 1
  await RisingEdge(dut.clk)
  await Timer(1, units="ns")
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.sram_odata.value.integer:08x}")