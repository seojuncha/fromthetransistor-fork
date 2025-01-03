import sys
import os

as_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "assembler"))
sys.path.append(as_path)
from as_parser import Parser
from as_encoder import Encoder

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

@cocotb.test()
async def tb_barrel_shifter(dut):
  # Prepare test data
  assembly_code_snippet = [
    "mov r0, #1",       # e3a00001
    "mov r1, #0xff",    # e3a010ff
    "mov r3, r0",       # e1a03000
    "mov r4, r1",       # e1a04001
    "mov r5, r3, lsl #1",  # e1a05083
    "mov r6, r4, lsl #3"   # e1a06184
  ]

  # parser(assembly code) -> assembler obj -> encoder(assembler obj) -> binary encode shfiter operand (12-bit)
  p = Parser()
  e = Encoder()
  for code in assembly_code_snippet:
    obj = p.parse_line(code)
    e.encoding_bits = 0x00000000
    e.set_condition_flag_bit("al")
    e.data_processing_encoding(obj)
    dut._log.info(f"{code:<20} >>> 0x{e.encoding_bits:08x}")
    e.to_little_endian()
    dut._log.info(f"little endian:       >>> 0x{e.encoding_bits:08x}\n")

  c = Clock(dut.clk, 10, units="ns")
  cocotb.start_soon(c.start())

  for i in range(10):
    await FallingEdge(dut.clk)