#!/usr/bin/env python3

import os

from elftools.elf.elffile import ELFFile
from capstone import Cs, CS_ARCH_ARM, CS_MODE_ARM

condition_code_map = {
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

data_opcode_map = {
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

shift_code_map = {
  "lsl": 0b00, # Logical Shift Left
  "lsr": 0b01, # Logical Shift Right
  "asr": 0b10, # Arithmetic Shift Right
  "ror": 0b11, # Rotate Right 
}
# "rrx"  # Rotate Right with Extend

data_processing_type1_list = ["mov", "mvn"]
data_processing_type2_list = ["cmp", "cmn", "tst", "teq"]
data_processing_type3_list = ["add", "sub", "rsb", "adc", "sbc", "rsc", "and", "bic", "eor", "orr"]

branch_instruction_list = ["b", "bl", "blx", "bx"]
multiply_instruction_list = ["mla", "mul", "smlal", "smull", "umlal", "umull"]
memory_instruction_list = ["ldr", "ldrb", "ldrbt", "ldrh", "ldrsb", "ldrsh", "ldrt", 
                           "str", "strb", "strbt", "strh", "strt"]

class ShifterOperandObj:
  def __init__(self, shifter_operand: str):
    self.is_only_imm = False
    self.is_only_reg = False
    self.is_rrx = False
    self.rm = 0
    self.imm = 0
    self.shifter = None
    self.shift_imm = 0
    self.rs = 0

    operands = [ opr.strip() for opr in shifter_operand.split(",") ]

    if len(operands) == 1:
      if operands[0].startswith("#"):
        self.is_only_imm = True
        self.imm = int(operands[0][1:])
      else:
        self.is_only_reg = True
        self.rm = int(operands[0][1:])
    else:
      self.rm = int(operands[0][1:])
      shifter_split = operands[1].split(" ")
      if len(shifter_split) == 1:
        self.is_rrx = True
      else:
        self.shifter = shifter_split[0]
        if shifter_split[1].startswith("#"):
          self.shift_imm = int(shifter_split[1][1:])
        else:
          self.rs = int(shifter_split[1][1:])

  def dump(self):
    print(f"imm\t[#{self.imm}]\t\trm\t[R{self.rm}]\nshifter\t[{self.shifter}]\t\timm\t[#{self.shift_imm}]\trs\t[R{self.rs}]")


class InstructionObj:
  @classmethod
  def create(cls, line_number: int, line_string: str):
    print(f"CREATE [line:{line_number:>3d}]\t{line_string:<80}")
    line_elem = line_string.split(" ")
    mnemonic = line_elem[0].casefold()

    if mnemonic in data_opcode_map.keys():
      num = int(line_elem[1].rstrip(",")[1:])
      if mnemonic in data_processing_type1_list:
        return cls(line_string, mnemonic, rd=num, shifter_operand=line_elem[2])
      elif mnemonic in data_processing_type2_list:
        return cls(line_string, mnemonic, rn=num, shifter_operand=line_elem[2])
      elif mnemonic in data_processing_type3_list:
        num2 = int(line_elem[2].rstrip(",")[1:])
        return cls(line_string, mnemonic, rd=num, rn=num2, shifter_operand=line_elem[3])
      else:
        print(f"ERROR 1 : {mnemonic}")
        return None
    elif mnemonic in branch_instruction_list:
      if len(line_elem) != 2:
        print(f"ERROR 2 : {mnemonic}")
        return None
      return cls(line_string, mnemonic, label=line_elem[1])
    elif mnemonic in multiply_instruction_list:
      return cls(line_string, mnemonic)
    elif mnemonic in memory_instruction_list:
      return None
    else:
      return None
      
  def __init__(self, line_string: str, name: str, label: str=None, rd: str=None, rn: str=None, shifter_operand: str=None):
    self.name = name
    self.line_string = line_string
    self.has_condition = False
    self.label = label
    self.rd = rd
    self.rn = rn
    self.shifter_obj = None if shifter_operand == None else ShifterOperandObj(shifter_operand)

  def is_branch(self):
    return self.label != None
  
  def is_memory(self):
    pass

  def is_multiply(self):
    pass

  def is_type1_dataprocessing(self):
    return self.rd != None and self.shifter_obj != None

  def is_type2_dataprocessing(self):
    return self.rn != None and self.shifter_obj != None

  def is_type3_dataprocessing(self):
    return self.rd != None and self.rn != None and self.shifter_obj != None
  
  def dump(self):
    print(f"{self.line_string:^80}")
    rd = f"R{self.rd}" if self.rd is not None else "-"
    rn = f"R{self.rn}" if self.rn is not None else "-"
    label = f"{self.label}" if self.label is not None else "-"
    print(f"rd\t[{rd}]\t\trn\t[{rn}]\tlabel\t[{label}]")
    if self.shifter_obj != None:
      self.shifter_obj.dump()
    print("="*80)

class Parser:
  def __init__(self, filepath=None):
    self._file = open(filepath, "r")
    self._not_support_count = 0

  def parse(self) -> list[InstructionObj]:
    objs = []
    for i, line in enumerate(self._file.readlines()):
      ret, line = Parser.cleaner(line)
      if ret: continue

      line = Parser.remove_line_comment(line)
      if Parser.is_directives(line):
        pass
      elif Parser.is_label(line):
        print(f"not support yet:\t{line}")
        self._not_support_count += 1
      else:
        objs.append(InstructionObj.create(i+1, line))

    self._file.close()

    return objs

  @staticmethod
  def cleaner(line: str):
    return ((line.isspace() or len(line) == 0 or line.startswith(("@",";"))), line.rstrip())
  
  @staticmethod
  def is_directives(line: str):
    return line.startswith(".")
  
  @staticmethod
  def is_label(line: str):
    return line.endswith(":")
  
  @staticmethod
  def remove_line_comment(line: str):
    return line.split("@")[0].strip()

class Encoder:
  def __init__(self):
    self.encoding_bits = 0x00000000

  def encode(self, objs: list[InstructionObj]):
    for obj in objs:
      # Now, use only always condition
      self.set_condition_flag_bit("al")
      if obj.is_type1_dataprocessing() or obj.is_type2_dataprocessing() or obj.is_type3_dataprocessing():
        self.data_processing_encoding(obj)

  def data_processing_encoding(self, obj: InstructionObj):
    self.set_bit(0, 27)
    self.set_bit(0, 26)
    if obj.is_type1_dataprocessing:
      self.set_bit(data_opcode_map[obj.name], 21)
      self.set_bit(0, 20) # s bit, now 0
      self.set_bit(0, 16)
      # TODO next!

  def branch_encoding(self):
    pass

  def memory_encoding(self):
    pass

  def multiply_encoding(self):
    pass

  def set_condition_flag_bit(self, name: str):
    self.encoding_bits |= condition_code_map[name] << 28

  def set_bit(self, val, pos):
    self.encoding_bits |= val << pos

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

  p = Parser("sample/from-chatgpt/data_processing.s")
  objs = p.parse()

  for o in objs:
    o.dump()

  # e = Encoder()
  # e.encode