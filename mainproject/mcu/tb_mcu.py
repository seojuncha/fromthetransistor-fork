import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def tb_mcu(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await Timer(10, units="ns")

  # wait for reset
  dut.rst.value = 1
  await RisingEdge(dut.clk)

  cpu_core = dut.cpu_core
  mem_ctrl = dut.mem_ctrl

  for i in range(15):
    dut._log.info(f"address register: 0x{dut.cpu_core.address_register.value.integer:08x}")
    dut._log.info(f"read data register: 0x{dut.cpu_core.read_data_register.value.integer:08x}")
    dut._log.info(f"write data register: 0x{dut.cpu_core.write_data_register.value.integer:08x}")
    await RisingEdge(dut.clk)
