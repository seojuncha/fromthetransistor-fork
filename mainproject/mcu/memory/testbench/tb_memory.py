import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

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
  # has_error = lambda error: "ERROR" if error else "SUCCESS"

  dut.addr.value = 0x0000_0000
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0000_ffff
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0001_0000
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0001_ffff
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0002_0000
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0002_ffff
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0003_0000
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0003_ffff
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0004_0000
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
  dut.addr.value = 0x0000_0000
  dut.idata_from_cpu.value = 2**32 - 1
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} <- 0x{dut.bram_mem.idata.value.integer:08x}")


  dut.addr.value = 0x0001_0000
  dut.idata_from_cpu.value = (2**32 - 1) >> 16
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} <- 0x{dut.sram_mem.idata.value.integer:08x}")


  dut.cpu_read_mem.value = 1
  dut.addr.value = 0x0000_0000
  # CHCKE: Always need two clock cycles?
  await RisingEdge(dut.clk)
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.odata_to_cpu.value.integer:08x}")

  dut.addr.value = 0x0001_0000
  await RisingEdge(dut.clk)
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.odata_to_cpu.value.integer:08x}")

  dut.addr.value = 0x0001_0001
  await RisingEdge(dut.clk)
  await RisingEdge(dut.clk)
  dut._log.info(f"[{has_error(dut)}] {selected_memory_type(dut)} -> 0x{dut.odata_to_cpu.value.integer:08x}")