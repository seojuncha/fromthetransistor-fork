from as_obj import *
from define import *

class Parser:
  def __init__(self, filepath):
    self._file = open(filepath, "r")
    self._not_support_count = 0
    self._inst_num = 0

  def parse(self) -> list[InstructionObj]:
    inst_objs = []
    for line in self._file.readlines():
      ret, line = Parser.cleaner(line)
      if ret: continue

      line = Parser.remove_line_comment(line)
      if Parser.is_directives(line):
        self._not_support_count += 1
      elif Parser.is_label(line):
        self._not_support_count += 1
      else:
        inst_addr = self._inst_num * 4
        line = " ".join(line.split())
        line_elem = line.split(" ")
        mnemonic = line_elem[0].casefold()

        print(line_elem)

        if mnemonic in data_opcode_map.keys():
          reg = line_elem[1].rstrip(",")
          if reg.casefold() == "pc":
            num = 15
          elif reg.casefold() == "lr":
            num = 14
          else:
            num = int(reg[1:])
          if mnemonic in data_processing_type1_list:
            inst_objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, shifter_operand=" ".join(line_elem[2:])))
          elif mnemonic in data_processing_type2_list:
            inst_objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rn=num, shifter_operand=" ".join(line_elem[2:])))
          elif mnemonic in data_processing_type3_list:
            num2 = int(line_elem[2].rstrip(",")[1:])
            if reg.casefold() == "pc":
              num2 = 15
            elif reg.casefold() == "lr":
              num2 = 14
            else:
              num2 = int(reg[1:])
            inst_objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, rn=num2, shifter_operand=" ".join(line_elem[3:])))
          else:
            print(f"ERROR 1 : {mnemonic}")
        elif mnemonic in branch_instruction_list:
          if len(line_elem) == 2:
            inst_objs.append(BranchInstObj(line, inst_addr, mnemonic, target_addr=line_elem[1]))
        elif mnemonic in multiply_instruction_list:
          rd = int(line_elem[1].rstrip(",")[1:])
          rm = int(line_elem[2].rstrip(",")[1:])
          rs = int(line_elem[3].rstrip(",")[1:])
          inst_objs.append(MultiplyInstObj(line, inst_addr, mnemonic, rd=rd, rm=rm, rs=rs))
        elif mnemonic in memory_instruction_list:
          rd = int(line_elem[1].rstrip(",")[1:])
          if line_elem[2].startswith("="):
            mov_imm = line_elem[2].replace("=", "#")
            inst_objs.append(DataProcessingInstObj(line, inst_addr, "mov", rd=rd, shifter_operand=mov_imm))
          else:
            inst_objs.append(MemoryInstObj(line, inst_addr, mnemonic, rd=rd, address_mode=line_elem[2].strip()))
        else:
          print("not found mnemonic: ", mnemonic)

        self._inst_num += 1

    self._file.close()

    return inst_objs

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
