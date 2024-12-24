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
  path = "sample/from-chatgpt/objdir/data_processing"
  with open(path+".o", "rb") as f:
    elf = ELFFile(f)
    text_section = elf.get_section_by_name(".text")
    text_data = text_section.data()