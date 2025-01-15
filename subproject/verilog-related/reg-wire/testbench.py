import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def testbench(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await RisingEdge(dut.clk)

  dut.rst.value = 1
  for i in range(5):
    await RisingEdge(dut.clk)
