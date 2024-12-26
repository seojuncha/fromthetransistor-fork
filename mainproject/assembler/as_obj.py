from define import *

class InstructionObj:
  @classmethod
  def create(cls, inst_number: int, line_string: str):
    inst_addr = inst_number * 4
    print(f"CREATE [addr: 0x{inst_addr:x}]\t{line_string:<80}")
    line_elem = line_string.split(" ")
    mnemonic = line_elem[0].casefold()

    if mnemonic in data_opcode_map.keys():
      num = int(line_elem[1].rstrip(",")[1:])
      if mnemonic in data_processing_type1_list:
        return cls(line_string, inst_addr, mnemonic, rd=num, shifter_operand=line_elem[2])
      elif mnemonic in data_processing_type2_list:
        return cls(line_string, inst_addr, mnemonic, rn=num, shifter_operand=line_elem[2])
      elif mnemonic in data_processing_type3_list:
        num2 = int(line_elem[2].rstrip(",")[1:])
        return cls(line_string, inst_addr, mnemonic, rd=num, rn=num2, shifter_operand=line_elem[3])
      else:
        print(f"ERROR 1 : {mnemonic}")
        return None
    elif mnemonic in branch_instruction_list:
      if len(line_elem) != 2:
        print(f"ERROR 2 : {mnemonic}")
        return None
      return cls(line_string, inst_addr, mnemonic, target_addr=line_elem[1])
    elif mnemonic in multiply_instruction_list:
      return cls(line_string, inst_addr, mnemonic)
    elif mnemonic in memory_instruction_list:
      return None
    else:
      return None
      
  def __init__(self, line_string: str, addr: int, name: str, target_addr: str=None, rd: int=0, rn: int=0, shifter_operand: str=None):
    self.name = name
    self.addr = addr
    self.line_string = line_string
    self.has_condition = False
    self.target_addr = None
    if target_addr:  self.target_addr = addr if target_addr is "." else int(target_addr[1:])
    self.rd = rd
    self.rn = rn
    self.shifter_obj = None if shifter_operand == None else ShifterOperandObj(shifter_operand)

  def is_branch(self):
    return self.target_addr != None
  
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
    print(f"[0x{self.addr:x}] {self.line_string:^50}")
    rd = f"R{self.rd}" if self.rd is not None else "-"
    rn = f"R{self.rn}" if self.rn is not None else "-"
    target_addr = f"0x{self.target_addr:x}" if self.target_addr is not None else "-"
    print(f"rd\t[{rd}]\t\trn\t[{rn}]\ttarget addr\t[{target_addr}]")
    if self.shifter_obj != None:
      self.shifter_obj.dump()
    print("="*60)


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
    print(f"imm\t[#{self.imm}]\t\trm\t[R{self.rm}]\nshifter\t[{self.shifter}]\t\tshift imm\t[#{self.shift_imm}]\trs\t[R{self.rs}]")