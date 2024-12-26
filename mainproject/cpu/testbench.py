import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer


@cocotb.test()
async def tb_cpu(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.n_reset.value = 0
  await Timer(10, units="ns")
  dut.n_reset.value = 1
  dut.bram_enable.value = 1

  for i in range(13):
    await FallingEdge(dut.clk)
    dut._log.info(f"[0x{int(dut.pc.value):<3x}] data_in [{dut.data_in.value}]")