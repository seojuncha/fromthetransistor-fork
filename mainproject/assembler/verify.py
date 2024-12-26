#!/usr/bin/env python3

import os

from elftools.elf.elffile import ELFFile
from capstone import Cs, CS_ARCH_ARM, CS_MODE_ARM

from as_parser import Parser
from as_encoder import Encoder

def check():
  with open("sample/from-chatgpt/objdir/data_processing.o", "rb") as of:
    elf = ELFFile(of)
    text_section = elf.get_section_by_name(".text")
    text_data = text_section.data()
    disassembler = Cs(CS_ARCH_ARM, CS_MODE_ARM)
    for i, instruction in enumerate(disassembler.disasm(text_data, text_section['sh_addr'])):
      print(f"0x{instruction.address:x}:\t{instruction.mnemonic}\t{instruction.op_str}")
      inst = (int.from_bytes(text_data, byteorder="little") >> (i * 32)) & 0xffff_ffff
      print(hex(inst))

if __name__ == "__main__":
  check()