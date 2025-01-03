import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

async def print_register_map(dut):
  dut._log.info(f"R0  [0x{dut.register[0].value.integer:x}]  R1 [0x{dut.register[1].value.integer:x}] R2 [0x{dut.register[2].value.integer:x}] R3 [0x{dut.register[3].value.integer:x}]")
  dut._log.info(f"R4  [0x{dut.register[4].value.integer:x}]  R5 [0x{dut.register[5].value.integer:x}] R6 [0x{dut.register[6].value.integer:x}] R7 [0x{dut.register[7].value.integer:x}]")
  dut._log.info(f"R8  [0x{dut.register[8].value.integer:x}]  R9 [0x{dut.register[9].value.integer:x}] R10 [0x{dut.register[10].value.integer:x}] R11 [0x{dut.register[11].value.integer:x}]")
  dut._log.info(f"R12 [0x{dut.register[12].value.integer:x}]  SP [0x{dut.sp.value.integer:x}] LR [0x{dut.lr.value.integer:x}] PC [0x{dut.pc.value.integer:x}]")

@cocotb.test()
async def tb_cpu(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.n_reset.value = 0
  await Timer(10, units="ns")
  dut.n_reset.value = 1
  dut.bram_enable.value = 1
  dut.little_endian.value = 1

  for i in range(13):
    await FallingEdge(dut.clk)
    dut._log.info(f"[0x{int(dut.pc.value):x}] data_in [{dut.data_in.value}]")
    await print_register_map(dut)