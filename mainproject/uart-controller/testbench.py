import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge

@cocotb.test()
async def tb_uart(dut):
  # 100MHz frequency
  c = Clock(dut.clk, 10, units="ns")
  dut._log.info("Clock Frequency: %f", c.frequency)

  cocotb.start_soon(c.start())

  # Clock frequency test
  for i in range(10):
    await RisingEdge(dut.clk)
    dut._log.info("clock: %d", dut.clk)

