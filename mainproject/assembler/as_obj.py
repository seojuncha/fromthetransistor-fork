from define import *

class InstructionObj:
  def __init__(self, 
               type: str,
               name: str, 
               addr: int,
               line_string: str):
    self.type = type
    self.addr = addr
    self.name = name
    self.line_string = line_string

  def dump(self):
    pass

class DataProcessingInstObj(InstructionObj):
  def __init__(self, 
               line_string: str, 
               addr: int, 
               name: str, 
               rd: int=0, 
               rn: int=0, 
               shifter_operand: str=None):
    super().__init__("data_processing", name, addr, line_string)
    self.has_condition = False
    self.rd = rd
    self.rn = rn
    self.shifter_obj = None if shifter_operand == None else ShifterOperandObj(shifter_operand)

  def dump(self):
    print(f"[0x{self.addr:x}] {self.line_string:^50}")
    rd = f"R{self.rd}" if self.rd is not None else "-"
    rn = f"R{self.rn}" if self.rn is not None else "-"
    print(f"rd\t[{rd}]\t\trn\t[{rn}]")
    if self.shifter_obj != None:
      self.shifter_obj.dump()
    print("="*60)

class BranchInstObj(InstructionObj):
  def __init__(self, 
               line_string: str, 
               addr: int, 
               name: str, 
               target_addr: str):
    super().__init__("branch", name, addr, line_string)
    self.target_addr = None
    if target_addr:  self.target_addr = addr if target_addr == "." else int(target_addr[1:])

  def dump(self):
    print(f"[0x{self.addr:x}] {self.line_string:^50}")
    target_addr = f"0x{self.target_addr:x}" if self.target_addr is not None else "-"
    print(f"target addr\t[{target_addr}]")

class MultiplyInstObj(InstructionObj):
  def __init__(self,
               line_string: str,
               addr: int,
               name: str,
               rd: int,
               rm: int,
               rs: int):
    super().__init__("multiply", name, addr, line_string)
    self.rd = rd
    self.rm = rm
    self.rs = rs

  def dump(self):
    print(f"[0x{self.addr:x}] {self.line_string:^50}")
    rd = f"R{self.rd}" if self.rd is not None else "-"
    rm = f"R{self.rm}" if self.rm is not None else "-"
    rs = f"R{self.rs}" if self.rs is not None else "-"
    print(f"rd\t[{rd}]\t\trm\t[{rm}]\t\trs\t[{rs}]")
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