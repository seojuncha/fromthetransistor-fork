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
  if cpu_core.state.value == 0b100:
    return "WRITE_BACK"
  if cpu_core.state.value == 0b101:
    return "DONE"
  return "UNKNOWN"

def print_register(cpu_core):
  cpu_core._log.info(f"R0 [0x{cpu_core.general_register[0].value.integer:x}]  R1 [0x{cpu_core.general_register[1].value.integer:x}] R2 [0x{cpu_core.general_register[2].value.integer:x}] R3 [0x{cpu_core.general_register[3].value.integer:x}]")
  cpu_core._log.info(f"R4 [0x{cpu_core.general_register[4].value.integer:x}]  R5 [0x{cpu_core.general_register[5].value.integer:x}] R6 [0x{cpu_core.general_register[6].value.integer:x}] R7 [0x{cpu_core.general_register[7].value.integer:x}]")
  cpu_core._log.info(f"R8 [0x{cpu_core.general_register[8].value.integer:x}]  R9 [0x{cpu_core.general_register[9].value.integer:x}] R10 [0x{cpu_core.general_register[10].value.integer:x}] R11 [0x{cpu_core.general_register[11].value.integer:x}]")
  cpu_core._log.info(f"R12 [0x{cpu_core.general_register[12].value.integer:x}]  R13 [0x{cpu_core.general_register[13].value.integer:x}] R14 [0x{cpu_core.general_register[14].value.integer:x}] PC [0x{cpu_core.pc.value.integer:x}]")

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

  for i in range(30):
    # dut._log.info(f"{state(cpu_core)} instruction : {cpu_core.instruction_register.value.integer:08x}")
    # dut._log.info(f"{state(cpu_core)} internal registers")
    # dut._log.info(f"addr [0x{cpu_core.address_register.value.integer:08x}] read [0x{cpu_core.read_data_register.value.integer:08x}] write [0x{cpu_core.write_data_register.value.integer:08x}]")
    # dut._log.info(f"{state(cpu_core)} bus")
    # dut._log.info(f"addr [0x{dut.address_bus.value.integer:08x}] data in [0x{dut.data_in_bus.value.integer:08x}]   data out [0x{dut.data_out_bus.value.integer:08x}]")
    # dut._log.info(f"{state(cpu_core)} cpu data")
    # dut._log.info(f"{state(cpu_core)} data in [0x{cpu_core.data_from_memory.value.integer:08x}] data out [0x{cpu_core.data_to_memory.value.integer:08x}]")

    # if cpu_core.state.value.integer >= 0b010:
    #   dut._log.info(f"rd {cpu_core.dec_rd.value}  rn {cpu_core.dec_rn.value}")
    await RisingEdge(dut.clk)
    print_register(cpu_core)
    # await Timer(1, units="ns")