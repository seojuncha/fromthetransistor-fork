from as_obj import *
from define import *

class Encoder:
  def __init__(self):
    self.encoding_bits = 0x00000000

  def encode(self, obj: InstructionObj):
    self.encoding_bits = 0x00000000
    # Now, use only always condition
    self.set_condition_flag_bit("al")
    if obj.type == "data_processing":
      self.data_processing_encoding(obj)
    elif obj.type == "branch":
      self.branch_encoding(obj)
    elif obj.type == "multiply":
      self.multiply_encoding(obj)
    elif obj.type == "memory":
      self.memory_encoding(obj)
    # print(f"0b{self.encoding_bits:32b} [0x{self.encoding_bits:4x}] {obj.line_string}")
    return obj.addr, self.encoding_bits

  def data_processing_encoding(self, obj: DataProcessingInstObj):
    self.set_bit(0, 27)
    self.set_bit(0, 26)

    self.set_bit(data_opcode_map[obj.name], 21)

    if obj.name == "teq" or obj.name == "tst" or obj.name == "cmp":
      self.set_bit(1,20)
    else:
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
    else:
      self.set_bit(0, 25)
      self.set_bit(obj.shifter_obj.rm, 0)
      if obj.shifter_obj.shifter == "lsl":
        if obj.shifter_obj.shift_imm:
          self.set_bit(obj.shifter_obj.shift_imm & 0x1f, 7)
        elif obj.shifter_obj.rs:
          self.set_bit(obj.shifter_obj.rs & 0xf, 8)
          self.set_bit(1, 4)
      elif obj.shifter_obj.shifter == "lsr":
        if obj.shifter_obj.shift_imm:
          self.set_bit(obj.shifter_obj.shift_imm & 0x1f, 7)
          self.set_bit(1, 5)
        elif obj.shifter_obj.rs:
          self.set_bit(obj.shifter_obj.rs & 0xf, 8)
          self.set_bit(1, 5)
          self.set_bit(1, 4)
      elif obj.shifter_obj.shifter == "asr":
        if obj.shifter_obj.shift_imm:
          self.set_bit(obj.shifter_obj.shift_imm & 0x1f, 7)
          self.set_bit(1, 6)
        elif obj.shifter_obj.rs:
          self.set_bit(obj.shifter_obj.rs & 0xf, 8)
          self.set_bit(1, 6)
          self.set_bit(1, 4)
      elif obj.shifter_obj.shifter == "ror":
        if obj.shifter_obj.shift_imm:
          self.set_bit(obj.shifter_obj.shift_imm & 0x1f, 7)
          self.set_bit(1, 6)
          self.set_bit(1, 5)
        elif obj.shifter_obj.rs:
          self.set_bit(obj.shifter_obj.rs & 0xf, 8)
          self.set_bit(1, 6)
          self.set_bit(1, 5)
          self.set_bit(1, 4)
      elif obj.shifter_obj.shifter == "rrx":
        self.set_bit(0b11, 5)

  def branch_encoding(self, obj: BranchInstObj):
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

  def multiply_encoding(self, obj: MultiplyInstObj):
    self.set_bit(0b0000000, 21)
    self.set_bit(0, 20) # depending on cpsr
    self.set_bit(obj.rd, 16)
    self.set_bit(0x0,12)
    self.set_bit(obj.rs, 8)
    self.set_bit(0b1001, 4)
    self.set_bit(obj.rm & 0xf, 0)

  def memory_encoding(self, obj: MemoryInstObj):
    self.set_bit(0b01, 26)  # only for word and unsigned byte
    self.set_bit(obj.rd, 12)

    if obj.is_load:
      self.set_bit(1, 20)  # L bit
    if obj.addr_mod_obj.is_need_write_back:
      self.set_bit(1, 21)  # W bit
    if obj.addr_mod_obj.is_unsigned_byte:
      self.set_bit(1, 22)  # B bit
    if obj.addr_mod_obj.is_add_offset:
      self.set_bit(1, 23)  # U bit
    if obj.addr_mod_obj.is_not_post_index:
      self.set_bit(1, 24)  # P bit
    
    self.set_bit(obj.addr_mod_obj.rn & 0xf, 16)

    if obj.addr_mod_obj.type == "base":
      return

    if obj.addr_mod_obj.type == "offset":
      if obj.addr_mod_obj.is_imm_type:
        self.set_bit(obj.addr_mod_obj.imm_12 & 0xfff, 0)
      elif obj.addr_mod_obj.is_reg_type:
        self.set_bit(obj.addr_mod_obj.rm & 0xf, 0)
      else:
        self.set_bit(obj.addr_mod_obj.shift_imm & 0x1f, 7)
        self.set_bit(shift_code_map[obj.addr_mod_obj.shifter], 5)
        self.set_bit(obj.addr_mod_obj.rm & 0xf, 0)


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
