import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

def state(cpu_core):
  if cpu_core.state.value == 0b000:
    return "IDLE"
  if cpu_core.state.value == 0b001:
    return "FETCH"
  if cpu_core.state.value == 0b010:
    return "DECODE"
  if cpu_core.state.value == 0b011:
    return "EXECUTE"
  return "UNKNOWN"


@cocotb.test()
async def tb_mcu(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.rst.value = 0
  await Timer(10, units="ns")

  # wait for reset
  dut.rst.value = 1
  dut.data_in_bus.value = 0
  dut.data_out_bus.value = 0
  dut.address_bus.value = 0
  await RisingEdge(dut.clk)

  cpu_core = dut.cpu_core
  mem_ctrl = dut.mem_ctrl

  for i in range(11):
    dut._log.info(f"{state(cpu_core)} instruction : {cpu_core.instruction_register.value.integer:08x}")
    dut._log.info(f"{state(cpu_core)} internal registers")
    dut._log.info(f"addr [0x{cpu_core.address_register.value.integer:08x}] read [0x{cpu_core.read_data_register.value.integer:08x}] write [0x{cpu_core.write_data_register.value.integer:08x}]")
    dut._log.info(f"{state(cpu_core)} bus")
    dut._log.info(f"addr [0x{dut.address_bus.value.integer:08x}] data in [0x{dut.data_in_bus.value.integer:08x}]   data out [0x{dut.data_out_bus.value.integer:08x}]")
    dut._log.info(f"{state(cpu_core)} cpu data")
    dut._log.info(f"{state(cpu_core)} data in [0x{cpu_core.data_from_memory.value.integer:08x}] data out [0x{cpu_core.data_to_memory.value.integer:08x}]")
    await RisingEdge(dut.clk)
    await Timer(1, units="ns")
