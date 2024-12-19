import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

def char_to_bin(c):
  return format(ord(c), '08b')

@cocotb.test()
async def fsm_tb(dut):

  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await FallingEdge(dut.rst)
  await Timer(5, units="ns")
  dut.rst.value = 1
  dut.start.value = 1
  await RisingEdge(dut.clk)

  binchar = char_to_bin("a")
  print(f"binchar: {binchar}")
  for i, b in enumerate(binchar):
    dut.data_in.value = int(b)
    await RisingEdge(dut.clk)
    dut._log.info("[%d][%s][%s] data_in: %s, data_out: %s", i, dut.state.value, dut.done.value, dut.data_in.value, dut.data_out.value.binstr)

  dut.data_in.value = 0
  await RisingEdge(dut.done)
  dut._log.info("[%d][%s][%s] data_in: %s, data_out: %s", i, dut.state.value, dut.done.value, dut.data_in.value, dut.data_out.value.binstr)