#!/usr/bin/env python3

import os

from elftools.elf.elffile import ELFFile
from capstone import Cs, CS_ARCH_ARM, CS_MODE_ARM

"""
ver 1:
 do
- only encode a instruction line
 test
- compare the output of own as with the original's

Contents of the line
- directives
- instructions
  - start with indentation
- labels
- comments
- data directives
"""

cond_map = {
  "eq": 0b0000,
  "ne": 0b0001,
  "cs": 0b0010,
  "cc": 0b0011,
  "mi": 0b0100,
  "pl": 0b0101,
  "vs": 0b0110,
  "vc": 0b0111,
  "hi": 0b1000,
  "ls": 0b1001,
  "ge": 0b1010,
  "lt": 0b1011,
  "gt": 0b1100,
  "le": 0b1101,
  "al": 0b1110,
  "nv": 0b1111,
}

opcode_map = {
  "and": 0b0000,
  "eor": 0b0001,
  "sub": 0b0010,
  "rsb": 0b0011,
  "add": 0b0100,
  "adc": 0b0101,
  "sbc": 0b0110,
  "rsc": 0b0111,
  "tst": 0b1000,
  "teq": 0b1001,
  "cmp": 0b1010,
  "cmn": 0b1011,
  "orr": 0b1100,
  "mov": 0b1101,
  "bic": 0b1110,
  "mvn": 0b1111,
}

shift_map = {
  "lsl": 0b00, # Logical Shift Left
  "lsr": 0b01, # Logical Shift Right
  "asr": 0b10, # Arithmetic Shift Right
  "ror": 0b11, # Rotate Right 
}
# "rrx"  # Rotate Right with Extend

def bits(v, p):
  return (v << p)

def process_instruction():
  # remove comments
  inst_line = line.split("@")[0].strip()
  # print(f"  INST: {str.encode(inst_line)}")
  sep_inst = inst_line.split(" ")
  if len(sep_inst) < 3:
    print(f" PARSE ERROR: {inst_line}")

  # Data Processing
  # cond[31:28]
  # 27: 0, 26: 0, 25: (1: imm, 0: not imm)
  # opcode[24:21]
  # s[20]
  # rn[19:16]
  # rd[15:12]
  # operand2[11:0]
  # TODO: Add for Multiple
  opcode = sep_inst[0].casefold()
  ops = [ ops.strip(", ") for ops in sep_inst[1:] ]
  print(f"\t {opcode}\t{ops}")
  is_imm = False

  # opcode
  if opcode[0:3] not in opcode_map.keys():
    print(f"OPCODE ERROR: {opcode}")

  # 1. move instruction (mov rd, <operand2>)
  if opcode == "mov":
    rd = ops[0]
    operand2 = ops[1:]
    # rd (r0 -> 0x0, r1 -> 0x1, r2 -> 0x2)
    rd_num = int(rd[1:])
    # operand2
    # immediate?
    if operand2[0].startswith("#"):
      # 32bit-imm
      imm = int(operand2[0][1:])
      is_imm = True
      print(f"imm: {imm}")
    else:
      rm = operand2[0]
      if len(operand2) == 1:
        rm_num = int(rm[1:])
        print(f"only register: {rm_num}")
      elif len(operand2) == 2:
        # rm, rrx
        if operand2[1] != "rrx":
          print("not rrx!")
      elif len(operand2) == 3:
        # rm, {shift} imm
        if operand2[2].startswith("#"):
          shift_imm = int(operand2[2][1:])
        # rm, {shift} rs
        else:
          rs_num = int(operand2[2][1:])
      else:
        print("ERROR")
    # make a bit stream
    encoded = bits(cond_map["al"], 28) | bits(0, 27) | bits(0, 26)
    if is_imm:
      encoded |= bits(1, 25)
    else:
      encoded |= bits(0, 25)
    encoded |= bits(opcode_map["mov"],21) | bits(0, 20) | bits(0, 16) | bits(rd_num, 12)
    # TODO: Rotate imm?
    encoded |= bits(0, 9) | bits(imm, 0)
    print(f"encoded binary: {bin(encoded)} [{hex(encoded)}]")

  # 2. add instruction (add rd, rn, <operand2>)
  elif opcode == "add":
    pass
  else:
    print(f"NOT SUPPORT ERROR: {inst_line}")

def check():
  with open("sample/from-chatgpt/objdir/data_processing.o", "rb") as of:
    elf = ELFFile(of)
    text_section = elf.get_section_by_name(".text")
    print(text_section)
    print(f"section info: {text_section['sh_info']}")
    print(f"section name: {text_section['sh_name']}")
    print(f"section type: {text_section['sh_type']}")
    print(f"section size: {text_section['sh_size']}")
    print(f"section offset: {text_section['sh_offset']}")
    print(f"section addr: {text_section['sh_addr']}")
    print(f"section link: {text_section['sh_link']}")

    text_data = text_section.data()
    print(text_data)

    disassembler = Cs(CS_ARCH_ARM, CS_MODE_ARM)
    for instruction in disassembler.disasm(text_data, text_section['sh_addr']):
      print(f"0x{instruction.address:x}:\t{instruction.mnemonic}\t{instruction.op_str}")


if __name__ == "__main__":
  enable_check = os.getenv("CHECK", 0)
  if enable_check == "1":
    check()

  with open("sample/from-chatgpt/data_processing.s", "r") as f:
    for line in f.readlines():
      line = line.rstrip()
      if line.isspace() or len(line) == 0:
        continue

      # instructions have indentation.
      if len(line) - len(line.lstrip()) > 0:
        process_instruction()
      elif line.startswith("."):
        print(f"{'DIRECTIVES':<40}{line:>20}")
      elif line.startswith("@"):
        print(f"{'LINE COMMENTS':<40}{line:>20}")
      else:
        print(f"{'NOT SUPPORT LINE':<40}{line:>20}")