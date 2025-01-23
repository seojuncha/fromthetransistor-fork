#!/usr/bin/env python3

import os

from elftools.elf.elffile import ELFFile
from capstone import Cs, CS_ARCH_ARM, CS_MODE_ARM

from as_parser import Parser
from as_encoder import Encoder

def build_reference():
  os.system("")

def check(path):
  comp1 = dict()
  with open(path+".o", "rb") as of:
    elf = ELFFile(of)
    text_section = elf.get_section_by_name(".text")
    text_data = text_section.data()
    disassembler = Cs(CS_ARCH_ARM, CS_MODE_ARM)
    for i, instruction in enumerate(disassembler.disasm(text_data, text_section['sh_addr'])):
      inst = (int.from_bytes(text_data, byteorder="little") >> (i * 32)) & 0xffff_ffff
      comp1[instruction.address] = (f"{instruction.mnemonic} {instruction.op_str}", inst)

  comp2 = dict()
  p = Parser()
  objs = p.parse(path+".s")
  e = Encoder()
  for o in objs:
    addr, bits = e.encode(o)
    comp2[addr] = bits

  success = []
  fail = []
  for address, (opstr, instruction) in comp1.items():
    if address in comp2.keys():
      if instruction != comp2.get(address):
        fail.append(f"[{opstr:<14}]\t{instruction:8x}\t{comp2.get(address):8x}")
      else:
        success.append(f"[{opstr:<14}]\t{instruction:8x}\t{comp2.get(address):8x}")
    else:
      fail.append(f"no address error: [{address}] {opstr}")

  print("="*20+"SUCCESS: "+str(len(success))+"="*20)
  for s in success:
    print(s)
  print("="*20+"FAIL: "+str(len(fail))+"="*20)
  for f in fail:
    print(f)

if __name__ == "__main__":
  # check("sample/from-chatgpt/branch")
  # check("sample/from-claude/arm-test-data")
  # check("sample/data1")
