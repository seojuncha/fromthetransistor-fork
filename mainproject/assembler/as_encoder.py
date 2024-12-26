from as_obj import InstructionObj
from define import *

class Encoder:
  def __init__(self):
    self.encoding_bits = 0x00000000

  def encode(self, obj: InstructionObj):
    self.encoding_bits = 0x00000000
    # Now, use only always condition
    self.set_condition_flag_bit("al")
    if obj.is_type1_dataprocessing() or obj.is_type2_dataprocessing() or obj.is_type3_dataprocessing():
      self.data_processing_encoding(obj)
    elif obj.is_branch():
      self.branch_encoding(obj)
    # print(f"0b{self.encoding_bits:32b} [0x{self.encoding_bits:4x}] {obj.line_string}")
    return obj.addr, self.encoding_bits

  def data_processing_encoding(self, obj: InstructionObj):
    self.set_bit(0, 27)
    self.set_bit(0, 26)

    self.set_bit(data_opcode_map[obj.name], 21)
    self.set_bit(0, 20) # s bit, now 0

    self.set_bit(obj.rn, 16)
    self.set_bit(obj.rd, 12)

    if obj.shifter_obj.is_only_imm:
      self.set_bit(1, 25)
      self.set_rotate_imm(obj.shifter_obj.imm)
    elif obj.shifter_obj.is_only_reg:
      self.set_bit(0, 25)
      self.set_bit(0x00, 4)
      self.set_bit(obj.shifter_obj.rm, 0)

  def branch_encoding(self, obj: InstructionObj):
    self.set_bit(1, 27)
    self.set_bit(0, 26)
    self.set_bit(1, 25)
    self.set_bit(0, 24) # no link address now.
    # target address = PC + 8 + (offset * 4)
    # offset = target address - (PC + 8) / 4
    # if target address is the address of branch instruction
    # offset = pc - pc - 8 / 4 = -2
    # two's comp of -2 (4-bit) = 0010 -> 1101 -> 1110!
    # 24-bit sign extended : 111...1110 -> 0xfffffe 
    self.set_bit(0xfffffe, 0)  # TEMP

  def memory_encoding(self):
    pass

  def multiply_encoding(self):
    pass

  def set_condition_flag_bit(self, name: str):
    self.encoding_bits |= condition_code_map[name] << 28

  def set_bit(self, val, pos):
    self.encoding_bits |= val << pos

  def set_rotate_imm(self, imm):
    # rotate_imm[11:8] - 4 bits
    # immed_8[7:0] - 8 bits
    # 4-bit rotate_imm : 16 cases
    for i in range(16):
      # get 8-bit to find a proper immed_8 value
      imm8 = (imm >> (2*i)) & 0xff
      # must represent a value in 32-bit word
      rotated_imm = ((imm >> (2*i)) | (imm << 32-(2*i))) & 0xffffffff
      if rotated_imm == imm:
        self.set_bit(i, 8)
        self.set_bit(imm8, 0)
        break
