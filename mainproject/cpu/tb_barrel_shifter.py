import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def tb_barrel_shifter(dut):
  # Prepare test data
  assembly_shift_operand_imm32 = (
    "#1\n"
    "#0xff\n"
  )
  assembly_shift_operand_reg = (
    "r0\n"
    "r1\n"
  )
  assembly_shift_operand_lsl_imm = (
    "r0, lsl #1\n"
    "r1, lsl #3\n"
  )
  assembly_shift_operand_lsl_rs = ()
  assembly_shift_operand_lsr_imm = ()
  assembly_shift_operand_lsr_rs = ()
  assembly_shift_operand_asr_imm = ()
  assembly_shift_operand_asr_rs = ()
  assembly_shift_operand_ror_imm = ()
  assembly_shift_operand_ror_rs = ()
  assembly_shift_operand_rrx = ()

  # parser(assembly code) -> assembler obj -> encoder(assembler obj) -> binary encode shfiter operand (12-bit)

  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  for i in range(10):
    await FallingEdge(dut.clk)