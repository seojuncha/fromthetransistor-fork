import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def tb_barrel_shifter(dut):
  # Prepare test data
  assembly_shift_operand_imm32 = (
    "mov r0, #1\n"
    "mov r1, #0xff\n"
  )
  assembly_shift_operand_reg = (
    "mov r3 r0\n"
    "mov r4 r1\n"
  )
  assembly_shift_operand_lsl_imm = (
    "mov r5, r3, lsl #1\n"
    "mov r6, r4, lsl #3\n"
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