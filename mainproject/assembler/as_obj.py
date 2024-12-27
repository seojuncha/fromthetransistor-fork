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
    print("="*60)

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
    self.shift_imm = None
    self.rs = None

    operands = [ opr.strip() for opr in shifter_operand.split(",") ]

    print("OOOOO:", operands)

    if len(operands) == 1:
      if operands[0].startswith("#"):
        self.is_only_imm = True
        imm = operands[0][1:]
        if imm.startswith("0x"):
          self.imm = int(imm, 16)
        elif imm.startswith("0b"):
          self.imm = int(imm, 2)
        else:
          self.imm = int(imm)
      else:
        self.is_only_reg = True
        if operands[0].casefold() == "pc":
          self.rm = 15
        elif operands[0].casefold() == "lr":
          self.rm = 14
        else:
          self.rm = int(operands[0][1:])
    else:
      if operands[0].casefold() == "pc":
          self.rm = 15
      elif operands[0].casefold() == "lr":
        self.rm = 14
      else:
        self.rm = int(operands[0][1:])

      shifter_split = operands[1].split(" ")
      print("shifter split:", shifter_split)
      if len(shifter_split) == 1:
        self.is_rrx = True
      else:
        self.shifter = shifter_split[0].casefold()
        if shifter_split[1].startswith("#"):
          self.shift_imm = int(shifter_split[1][1:])
        else:
          self.rs = int(shifter_split[1][1:])

  def dump(self):
    print(f"imm\t[#{self.imm}]\t\trm\t[R{self.rm}]\nshifter\t[{self.shifter}]\t\tshift imm\t[#{self.shift_imm}]\trs\t[R{self.rs}]")

class MemoryInstObj(InstructionObj):
  def __init__(self,
               line_string: str,
               addr: int,
               name: str,
               rd: int,
               address_mode: str):
    super().__init__("memory", name, addr, line_string)
    self.rd = rd  # Bit[15:12]
    self.is_load = True if name[:3] == "ldr" else False  # L bit(20)
    self.type_ver = None
    self.addr_mod_obj = AddressModeObj(address_mode)

  def dump(self):
    print(f"[0x{self.addr:x}] {self.line_string:^50}")
    print(f"rd\t[R{self.rd}]")
    self.addr_mod_obj.dump()
    print("="*60)

class AddressModeObj:
  """
   only base register
      [<Rn>]

   literal pool
      =<imm>

   nine offset addressing mode
   1. immediate offset/index
    offset
      [<Rn>, #+/-<offset_12>]
    pre-index
      [<Rn>, #+/-<offset_12>]!
    post-index
      [<Rn>], #+/-<offset_12>

   2. register offset/index
    offset
      [<Rn>, +/-<Rm>]
    pre-index
      [<Rn>, +/-<Rm>]!
    post-index
      [<Rn>], +/-<Rm>

   3. scaled register offset/index
    offset
      [<Rn>, +/-<Rm>, lsl #<shift_imm>]
      [<Rn>, +/-<Rm>, lsr #<shift_imm>]
      [<Rn>, +/-<Rm>, asr #<shift_imm>]
      [<Rn>, +/-<Rm>, ror #<shift_imm>]
      [<Rn>, +/-<Rm>, rrx]
    pre-index
      [<Rn>, +/-<Rm>, lsl #<shift_imm>]!
      [<Rn>, +/-<Rm>, lsr #<shift_imm>]!
      [<Rn>, +/-<Rm>, asr #<shift_imm>]!
      [<Rn>, +/-<Rm>, ror #<shift_imm>]!
      [<Rn>, +/-<Rm>, rrx]!
    post-index
      [<Rn>], +/-<Rm>, lsl #<shift_imm>
      [<Rn>], +/-<Rm>, lsr #<shift_imm>
      [<Rn>], +/-<Rm>, asr #<shift_imm>
      [<Rn>], +/-<Rm>, ror #<shift_imm>
      [<Rn>], +/-<Rm>, rrx
  """
  def __init__(self, addressing_mode: str):
    self.is_imm_type = False
    self.is_reg_type = False
    self.is_scaled_reg_type = False

    self.is_not_post_index = True   # P bit (24)
    self.is_add_offset = True   # U bit (23)
    self.is_unsigned_byte = False  # B bit (22)
    self.is_need_write_back = False # W bit (21)

    self.rn = None
    self.rm = None
    self.imm_12 = None
    self.shifter = None

    comma_split = addressing_mode.split(",")

    if len(comma_split) == 1:  # base mode
      self.type = "base"
      self.rn = int(comma_split[0][2:-1])
    else: # offset mode
      if comma_split[0][0] == "[":
        if comma_split[-1][-1] == "]":
          self.type = "offset"
          self.rn = int(comma_split[0][2:])
        elif comma_split[0][-1] == "]":
          self.type = "postindex"
          self.is_not_post_index = False
          self.rn = int(comma_split[0][2:-1])
        elif comma_split[-1].endswith("]!"):
          self.type = "preindex"
          self.rn = int(comma_split[0][2:])
      else:
        print("invalid syntax")
        return None
    
    if len(comma_split) > 2:
      elem = comma_split[1].strip()
      if elem.startswith("#"):
        self.is_imm_type = True
        if elem.startswith("#+"):
          self.imm_12 = int(elem[0][2:])
          if self.type == "offset":
            self.imm_12 = self.imm_12[:-1]
          if self.type == "preindex":
            self.imm_12 = self.imm_12[:-2]
        elif elem.startswith("#-"):
          self.imm_12 = int(elem[0][2:])
          if self.type == "offset":
            self.imm_12 = self.imm_12[:-1]
          if self.type == "preindex":
            self.imm_12 = self.imm_12[:-2]
        else:
          self.imm_12 = int(elem[0][1:])
      elif elem.casefold().startswith("r"):
        self.is_reg_type = True
        self.rm = int(elem[0][1:])
      elif elem.casefold().startswith("+r"):
        self.rm = int(elem[0][2:])
      elif elem.casefold().startswith("-r"):
        self.rm = int(elem[0][2:])
        self.is_add_offset = False

      if len(comma_split) == 3:
        elem2 = comma_split[2].strip()
        shifter_split = elem2.split(" ")
        self.shifter = shifter_split[0].strip()
        if len(shifter_split) > 1:
          self.shift_imm = int(shifter_split[1][1:])
        else:
          self.shift_imm = 0

  def dump(self):
    print(f"address type\t[{self.type}]")
    rn = f"R{self.rn}" if self.rn is not None else "-"
    rm = f"R{self.rm}" if self.rm is not None else "-"
    imm_12 = f"#{self.imm_12}" if self.imm_12 is not None else "-"
    print(f"rn\t[{rn}]\t\trm\t[{rm}]\t\timm\t[{imm_12}]")
    if self.shifter:
      print(f"shifter\t[{self.shifter}]\t\tshift imm\t[#{self.shift_imm}]")
