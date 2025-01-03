import sys
import os

as_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "assembler"))
sys.path.append(as_path)
from as_parser import Parser
from as_encoder import Encoder
from define import shift_code_map

import cocotb
from cocotb.triggers import Timer

# parser(assembly code) -> assembler obj -> encoder(assembler obj) -> binary encode shfiter operand (12-bit)
p = Parser()
e = Encoder()

async def test_imm32(dut):
  assembly_code_snippet = [
    "mov r0, #1",          # e3a00001
    "mov r1, #0xff",       # e3a010ff
  ]

  for code in assembly_code_snippet:
    obj = p.parse_line(code)
    e.encoding_bits = 0x00000000
    e.set_condition_flag_bit("al")
    e.data_processing_encoding(obj)
    dut._log.info(f"{code:<20} >>> 0x{e.encoding_bits:08x}")

    rotate_imm = (e.encoding_bits >> 8) & 0xf
    imm8 = e.encoding_bits & 0xff
    
    dut.is_imm_32.value = 1
    dut.is_use_rs.value = 0
    dut.carry_in.value = 0
    dut.shift_in.value = imm8
    dut.shift_imm.value = rotate_imm

    expected = ((imm8 >> (rotate_imm * 2)) | (imm8 << (32 - (rotate_imm * 2)))) & 0xffff_ffff

    await Timer(1, units="ns")

    result = "OK" if dut.shifter_operand.value == expected else "FAIL"
    dut._log.info(f"[{result}] C: {dut.shift_carry_out.value}  Operand: 0x{dut.shifter_operand.value.integer:08x} Expected: [0x{expected:08x}]")

async def test_reg(dut):
  assembly_code_snippet = [
    "mov r3, r0",          # e1a03000
    "mov r4, r1",          # e1a04001
  ]

  for code in assembly_code_snippet:
    obj = p.parse_line(code)
    e.encoding_bits = 0x00000000
    e.set_condition_flag_bit("al")
    e.data_processing_encoding(obj)
    dut._log.info(f"{code:<20} >>> 0x{e.encoding_bits:08x}")

    rm = e.encoding_bits & 0xf
    if rm == 0:
      rm_value = 0xffff_1111
    elif rm == 1:
      rm_value = 0xffff_2222

    dut.is_imm_32.value = 0
    dut.is_use_rs.value = 0
    dut.shift_imm.value = 0
    dut.shift_type.value = 0
    dut.carry_in.value = 0
    dut.shift_in.value = rm_value

    expected = rm_value & 0xffff_ffff

    await Timer(1, units="ns")

    result = "OK" if dut.shifter_operand.value == expected else "FAIL"
    dut._log.info(f"[{result}] C: {dut.shift_carry_out.value}  Operand: 0x{dut.shifter_operand.value.integer:08x} Expected: [0x{expected:08x}]")

async def test_shift_imm(dut):
  assembly_code_snippet = [
    "mov r5, r3, lsl #1",   # e1a05083
    "mov r6, r4, lsl #3",   # e1a06184
    "mov r5, r5, lsr #1",   # e1a050a5
    "mov r6, r6, lsr #3",   # e1a061a6
    # "mov r7, r6, asr #3",   # e1a071c6
    "mov r8, r5, ror #4"      # e1a08265
  ]

  for code in assembly_code_snippet:
    obj = p.parse_line(code)
    e.encoding_bits = 0x00000000
    e.set_condition_flag_bit("al")
    e.data_processing_encoding(obj)
    dut._log.info(f"{code:<20} >>> 0x{e.encoding_bits:08x}")

    shift_imm = (e.encoding_bits >> 7) & 0x1f
    shift = (e.encoding_bits >> 5) & 0b11
    rm = e.encoding_bits & 0xf

    if rm == 3:
      rm_value = 0xffff_3333
    elif rm == 4:
      rm_value = 0xffff_4444
    elif rm == 5:
      rm_value = 0x4fff_4444
    elif rm == 6:
      rm_value = 0x6fff_4444

    dut.is_imm_32.value = 0
    dut.is_use_rs.value = 0
    dut.is_imm_32.value = 0
    dut.carry_in.value = 0
    dut.shift_type.value = shift
    dut.shift_imm.value = shift_imm
    dut.shift_in.value = rm_value

    if shift == shift_code_map["lsl"]:
      expected = ((rm_value << shift_imm)) & 0xffff_ffff
    elif shift == shift_code_map["lsr"]:
      expected = ((rm_value >> shift_imm)) & 0xffff_ffff
    elif shift == shift_code_map["ror"]:
      expected = ((rm_value >> shift_imm) | (rm_value << (32 - shift_imm))) & 0xffff_ffff
    # TODO : ASR
      
    await Timer(1, units="ns")

    result = "OK" if dut.shifter_operand.value == expected else "FAIL"
    dut._log.info(f"[{result}] C: {dut.shift_carry_out.value}  Operand: 0x{dut.shifter_operand.value.integer:08x} Expected: [0x{expected:08x}]")

async def test_shift_reg(dut):
  assembly_code_snippet = [
    "mov r0, r0, lsl r1",
    "mov r1, r1, lsl r2",
  ]

  for code in assembly_code_snippet:
    obj = p.parse_line(code)
    e.encoding_bits = 0x00000000
    e.set_condition_flag_bit("al")
    e.data_processing_encoding(obj)
    dut._log.info(f"{code:<20} >>> 0x{e.encoding_bits:08x}")

    rs = (e.encoding_bits >> 8) & 0xf
    shift = (e.encoding_bits >> 5) & 0b11
    rm = e.encoding_bits & 0xf

    if rm == 0:
      rm_value = 0xffff_3333
    elif rm == 1:
      rm_value = 0xffff_4444
    elif rm == 5:
      rm_value = 0x4fff_4444
    elif rm == 6:
      rm_value = 0x6fff_4444

    if rs == 1:
      rs_value = 0xff44
    elif rs == 2:
      rs_value = 0xff55
    rs_value &= 0xff

    dut.is_imm_32.value = 0
    dut.is_use_rs.value = 1
    dut.is_imm_32.value = 0
    dut.carry_in.value = 0
    dut.shift_type.value = shift
    dut.rs.value = rs_value
    dut.shift_in.value = rm_value

    if shift == shift_code_map["lsl"]:
      if rs_value == 0:
        expected = rm_value
      elif rs_value < 32:
        expected = ((rm_value << rs_value)) & 0xffff_ffff
      elif rs_value == 32:
        expected = 0
    elif shift == shift_code_map["lsr"]:
      expected = ((rm_value >> rs_value)) & 0xffff_ffff
    elif shift == shift_code_map["ror"]:
      expected = ((rm_value >> rs_value) | (rm_value << (32 - rs_value))) & 0xffff_ffff
    # TODO : ASR
      
    await Timer(1, units="ns")

    result = "OK" if dut.shifter_operand.value == expected else "FAIL"
    dut._log.info(f"[{result}] C: {dut.shift_carry_out.value}  Operand: 0x{dut.shifter_operand.value.integer:08x} Expected: [0x{expected:08x}]")


@cocotb.test()
async def tb_barrel_shifter(dut):
  await test_imm32(dut)
  await test_reg(dut)
  await test_shift_imm(dut)