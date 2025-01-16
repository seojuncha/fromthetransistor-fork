import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

import os
import sys
import pathlib
as_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "assembler"))
sys.path.append(as_path)
from as_parser import Parser
from as_encoder import Encoder

async def print_register_map(dut):
  dut._log.info(f"R0 [0x{dut.register[0].value.integer:x}]  R1 [0x{dut.register[1].value.integer:x}] R2 [0x{dut.register[2].value.integer:x}] R3 [0x{dut.register[3].value.integer:x}]")
  dut._log.info(f"R4 [0x{dut.register[4].value.integer:x}]  R5 [0x{dut.register[5].value.integer:x}] R6 [0x{dut.register[6].value.integer:x}] R7 [0x{dut.register[7].value.integer:x}]")
  dut._log.info(f"R8 [0x{dut.register[8].value.integer:x}]  R9 [0x{dut.register[9].value.integer:x}] Ra [0x{dut.register[10].value.integer:x}] Rb [0x{dut.register[11].value.integer:x}]")
  dut._log.info(f"Rc [0x{dut.register[12].value.integer:x}]  SP [0x{dut.sp.value.integer:x}] LR [0x{dut.lr.value.integer:x}] PC [0x{dut.pc.value.integer:x}]")

"""
1. Instruction Fetch
bram -> cpu(data_in)
: not integrate bram in this stage.
: focibly push instructions through data_in of CPU

2. Instruction Decode
3. Instruction Execute
: check registers
: check control signals

"""
@cocotb.test()
async def tb_cpu(dut):
  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  dut.n_reset.value = 0
  await Timer(10, units="ns")

  # wait for reset
  dut.n_reset.value = 1
  dut.little_endian_en.value = 0
  await RisingEdge(dut.clk)

  p = Parser()
  e = Encoder()
  objs = p.parse("testdata/test1.s")

  for o in objs:
    addr, bits = e.encode(o)
    print(f"\t\t\t\t\t\t[0x{addr:x}] {bits:8x}")

    dut.data_in.value = bits

    # await FallingEdge(dut.clk)
    while True:
      await RisingEdge(dut.clk)
      if int(dut.state.value) == 0b101:
        break

    dut._log.info(f"dec_rn: {dut.dec_rn.value}, dec_rd: {dut.dec_rd.value}")
    dut._log.info(f"alu_out: {dut.alu_out.value}")

    await print_register_map(dut)