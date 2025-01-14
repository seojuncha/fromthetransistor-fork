import cocotb
from cocotb.triggers import Timer

import os
import sys

as_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "assembler"))
sys.path.append(as_path)
from define import data_opcode_map

# DATAWIDTH = 4

async def expected(opcode, a, b, carry_in):
  result = 0
  tmp = 0
  if opcode == "and":
    tmp = a & b
  elif opcode == "eor":
    tmp = a ^ b
  elif opcode == "orr":
    tmp = a | b
  elif opcode == "mov":
    tmp = b
  elif opcode == "mvn":
    tmp = ~b
  elif opcode == "bic":
    tmp = a & ~b
  elif opcode == "sub":
    tmp = a - b
  elif opcode == "rsb":
    tmp = b - a
  elif opcode == "add":
    tmp = a + b
  elif opcode == "adc":
    tmp = a + b + carry_in
  elif opcode == "sbc":
    tmp = a - b - ~carry_in
  elif opcode == "rsc":
    tmp = b - a - ~carry_in
  elif opcode == "tst":
    tmp = a & b
  elif opcode == "teq":
    tmp = a ^ b
  elif opcode == "cmp":
    tmp = a - b
  elif opcode == "cmn":
    tmp = a + b

  result = tmp & (2**DATAWIDTH - 1)

  flags = dict()
  flags["z"] = 1 if result == 0 else 0
  flags["n"] = 1 if (result & (1 << (DATAWIDTH - 1))) != 0 else 0

  if opcode == "and" or opcode =="eor" or opcode == "orr" or opcode =="mov" or opcode == "mvn" or opcode == "bic":
    flags["v"] = 0
    flags["c"] = carry_in
  else:
    flags["v"] = 1 if (a & b & ~result & (1<<(DATAWIDTH - 1))) or (~a & ~b & result & (1<<(DATAWIDTH - 1))) else 0
    if opcode == "sub" or opcode == "cmp" :
      flags["c"] = 1 if a >= b else 0
    elif opcode == "sbc":
      flags["c"] = 1 if a >= (b + carry_in) else 0
    elif opcode == "rsb":
      flags["c"] = 1 if b >= a else 0
    elif opcode == "rsc":
      flags["c"] = 1 if b >= (a + carry_in) else 0
    else:
      flags["c"] = 1 if tmp > (2**DATAWIDTH - 1) else 0

  return result, flags

@cocotb.test()
async def tb_alu(dut):
  global DATAWIDTH
  DATAWIDTH = dut.DATA_WIDTH.value

  dut.result.value = 0
  dut.zero_flag.value = 0
  dut.negative_flag.value = 0
  dut.carry_out_flag.value = 0
  dut.overflow_flag.value = 0
  dut.enable.value = 1

  for c in [0, 1]:
    carry_in = c & 0x1
    for k, v in data_opcode_map.items():
      if k != "mov": continue
      dut.opcode.value = v
      dut.carry_in.value = carry_in
      dut.enable_flag_update.value = 1   # forcibely now
      for i in range(2**DATAWIDTH):
        i &= (2**DATAWIDTH - 1)
        dut.operand1.value = i
        for j in range(2**DATAWIDTH):
          j &= (2**DATAWIDTH - 1)
          dut.operand2.value = j
          expected_result, expected_flags = await expected(k, i, j, carry_in)

          await Timer(1, "ns")

          alu_out = dut.result.value
          z_out = dut.zero_flag.value
          n_out = dut.negative_flag.value
          c_out = dut.carry_out_flag.value
          v_out = dut.overflow_flag.value

          alu_exp = expected_result
          z_exp = expected_flags["z"]
          n_exp = expected_flags["n"]
          c_exp = expected_flags["c"]
          v_exp = expected_flags["v"]

          out_fail = True if alu_out != alu_exp else False
          z_fail = True if z_out != z_exp else False
          n_fail = True if n_out != n_exp else False
          c_fail = True if c_out != c_exp else False
          v_fail = True if v_out != v_exp else False

          if out_fail or z_fail or n_fail or c_fail or v_fail:
            dut._log.info(f"\nOPCODE: {k} OPRD1: 0x{i:X} OPRD2: 0x{j:X} CARRY: {carry_in:b}")
            dut._log.info(f"Actual [Z:{z_out}] [N:{n_out}] [C:{c_out}] [V:{v_out}] 0x{alu_out.integer:X}")
            dut._log.info(f"Expect [Z:{z_exp}] [N:{n_exp}] [C:{c_exp}] [V:{v_exp}] 0x{alu_exp:X}")