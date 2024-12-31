from as_obj import *
from define import *

class Parser:
  def __init__(self, filepath):
    self._file = open(filepath, "r")
    self._not_support_count = 0
    self._inst_num = 0
    self._label_map = dict()

  def parse(self) -> list[InstructionObj]:
    self.parse_label()
    inst_objs = []
    for line in self._file.readlines():
      obj = self.parse_line(line)
      if obj is not None:
        inst_objs.append(obj)
    self._file.close()
    return inst_objs
  
  def parse_line(self, line: str) -> InstructionObj:
    invalid, line = Parser.cleaner(line)
    if invalid:
      return None
    line = Parser.remove_line_comment(line)
    if Parser.is_directives(line):
      self._not_support_count += 1
      return None
    if Parser.is_label(line):
      return None
    
    instobj = None
    inst_addr = self._inst_num * 4
    line = " ".join(line.split()).strip()
    line_elem = line.split(" ")
    mnemonic = line_elem[0].casefold()

    print(f"LINE: {line_elem}")

    if mnemonic in data_opcode_map.keys():
      reg = line_elem[1].rstrip(",")
      if reg.casefold() == "pc":
        num = 15
      elif reg.casefold() == "lr":
        num = 14
      else:
        num = int(reg[1:])
      if mnemonic in data_processing_type1_list:
        self._inst_num += 1
        instobj =  DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, shifter_operand=" ".join(line_elem[2:]))
      elif mnemonic in data_processing_type2_list:
        self._inst_num += 1
        instobj = DataProcessingInstObj(line, inst_addr, mnemonic, rn=num, shifter_operand=" ".join(line_elem[2:]))
      elif mnemonic in data_processing_type3_list:
        reg_rn = line_elem[2].rstrip(",")
        if reg_rn.casefold() == "pc":
          rn = 15
        elif reg_rn.casefold() == "lr":
          rn = 14
        else:
          rn = int(reg_rn[1:])
        self._inst_num += 1
        instobj = DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, rn=rn, shifter_operand=" ".join(line_elem[3:]))
      else:
        print(f"ERROR 1 : {mnemonic}")
    elif mnemonic in branch_instruction_list:
      if len(line_elem) == 2:
        branch_name = line_elem[1]
        if branch_name == ".":
          target_addr = inst_addr
        else:
          target_addr = self._label_map.get(branch_name)
        self._inst_num += 1
        instobj = BranchInstObj(line, inst_addr, mnemonic, target_addr=target_addr)
    elif mnemonic in multiply_instruction_list:
      rd = int(line_elem[1].rstrip(",")[1:])
      rm = int(line_elem[2].rstrip(",")[1:])
      rs = int(line_elem[3].rstrip(",")[1:])
      self._inst_num += 1
      instobj = MultiplyInstObj(line, inst_addr, mnemonic, rd=rd, rm=rm, rs=rs)
    elif mnemonic in memory_instruction_list:
      rd = int(line_elem[1].rstrip(",")[1:])
      if line_elem[2].startswith("="):
        mov_imm = line_elem[2].replace("=", "#")
        self._inst_num += 1
        instobj = DataProcessingInstObj(line, inst_addr, "mov", rd=rd, shifter_operand=mov_imm)
      else:
        self._inst_num += 1
        instobj = MemoryInstObj(line, inst_addr, mnemonic, rd=rd, address_mode=" ".join(line_elem[2:]))
    else:
      print(f"not found mnemonic => {mnemonic}")
      return None
    return instobj

  def parse_label(self):
    label_addr = 0
    if self._file is None:
      return
    for line in self._file.readlines():
      ret, line = Parser.cleaner(line)
      if ret: continue
      if Parser.is_directives(line):
        continue
      if not Parser.is_label(line):
        label_addr += 4
        continue
      line = Parser.remove_line_comment(line)
      self._label_map[line[:-1]] = label_addr
    self._file.seek(0)

  @staticmethod
  def cleaner(line: str):
    return ((line.isspace() or len(line) == 0 or line.strip().startswith(("@"))), line.rstrip())
  
  @staticmethod
  def is_directives(line: str):
    return line.startswith(".")
  
  @staticmethod
  def is_label(line: str):
    return line.endswith(":")
  
  @staticmethod
  def remove_line_comment(line: str):
    return line.split("@")[0].strip()
