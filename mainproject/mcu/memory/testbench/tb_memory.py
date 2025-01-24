import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

def selected_memory_type(dut):
  if dut.bram_selected.value == 1: mem_type = "bram"
  elif dut.sram_selected.value == 1: mem_type = "sram"
  elif dut.flash_selected.value == 1: mem_type = "flash"
  elif dut.peripheral_selected.value == 1: mem_type = "peripheral"
  else: mem_type = "unknown"
  return mem_type

async def test_address_decoding(dut):
  has_error = lambda error: "ERROR" if error else "SUCCESS"

  dut.addr.value = 0x0000_0000
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0000_ffff
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0001_0000
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0001_ffff
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0002_0000
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0002_ffff
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0003_0000
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")
  dut.addr.value = 0x0003_ffff
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  dut.addr.value = 0x0004_0000
  dut._log.info(f"[{has_error(dut.decode_error.value)}] {selected_memory_type(dut)}")
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

  # Should be "error" with the "unkown" memory type.
  has_error = "ERROR" if dut.error.value == 1 else "SUCCESS"
  dut._log.info(f"[{has_error}] {selected_memory_type(dut)}")
  await Timer(1, units="ns")

  await test_address_decoding(dut)
  

