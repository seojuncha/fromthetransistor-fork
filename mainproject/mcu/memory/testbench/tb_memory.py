import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def tb_memory(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await Timer(10, units="ns")

  # wait for reset
  dut.rst.value = 1
  await RisingEdge(dut.clk)

  
