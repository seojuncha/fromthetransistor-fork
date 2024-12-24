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