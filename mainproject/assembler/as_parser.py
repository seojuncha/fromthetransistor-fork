from as_obj import *
from define import *

class Parser:
  def __init__(self, filepath):
    self._file = open(filepath, "r")
    self._not_support_count = 0
    self._inst_num = 0

  def parse(self) -> list[InstructionObj]:
    objs = []
    for line in self._file.readlines():
      ret, line = Parser.cleaner(line)
      if ret: continue

      line = Parser.remove_line_comment(line)
      if Parser.is_directives(line):
        pass
      elif Parser.is_label(line):
        self._not_support_count += 1
      else:
        inst_addr = self._inst_num * 4
        # print(f"CREATE [addr: 0x{inst_addr:x}]\t{line_string:<80}")
        line_elem = line.split(" ")
        mnemonic = line_elem[0].casefold()

        if mnemonic in data_opcode_map.keys():
          num = int(line_elem[1].rstrip(",")[1:])
          if mnemonic in data_processing_type1_list:
            objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, shifter_operand=line_elem[2]))
          elif mnemonic in data_processing_type2_list:
            objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rn=num, shifter_operand=line_elem[2]))
          elif mnemonic in data_processing_type3_list:
            num2 = int(line_elem[2].rstrip(",")[1:])
            objs.append(DataProcessingInstObj(line, inst_addr, mnemonic, rd=num, rn=num2, shifter_operand=line_elem[3]))
          else:
            print(f"ERROR 1 : {mnemonic}")
            return None
        elif mnemonic in branch_instruction_list:
          if len(line_elem) != 2:
            print(f"ERROR 2 : {mnemonic}")
            return None
          objs.append(BranchInstObj(line, inst_addr, mnemonic, target_addr=line_elem[1]))
        elif mnemonic in multiply_instruction_list:
          rd = int(line_elem[1].rstrip(",")[1:])
          rm = int(line_elem[2].rstrip(",")[1:])
          rs = int(line_elem[3].rstrip(",")[1:])
          objs.append(MultiplyInstObj(line, inst_addr, mnemonic, rd=rd, rm=rm, rs=rs))
        elif mnemonic in memory_instruction_list:
          return None
        else:
          return None
        self._inst_num += 1

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
