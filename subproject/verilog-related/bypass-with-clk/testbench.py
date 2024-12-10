import cocotb
from cocotb.clock import Clock
from cocotb.triggers import FallingEdge

@cocotb.test()
async def testbench(dut):
  dut._log.info("falling edge test")
  dut._log.info("initial signal: %s", dut.output_signal.value)
  cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())
  for value in [0, 1, 0, 1, 1, 0]:
    dut.input_signal.value = value
    await FallingEdge(dut.clk)
    dut._log.info("int : %s out : %s", dut.input_signal.value, dut.output_signal.value)
