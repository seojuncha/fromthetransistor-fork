#!/usr/bin/env python3

import os

if __name__ == "__main__":
  os.system("make clean && make")

  with open("ex1.bin", "rb") as f:
    data = f.read()
    inst = int.from_bytes(data, "little")
    print("original: ", bin(inst))

    insts = []
    for i in range(4):
      insts.append((inst >> (32 * i)) & 0xffffffff)
      print(bin((inst >> (32 * i)) & 0xffffffff))
    print()

    for i in range(3):
      print("============", bin(insts[i]))
      print("condition: {0:{fill}4b}".format((insts[i] >> 28) & 0xf, fill="0"))
      print("opcode: {0:{fill}4b}".format((insts[i] >> 21) & 0xf, fill="0"))
      print("imm: {0:{fill}1b}".format((insts[i] >> 25) & 0x1, fill="0"))
      print("dest: {0:{fill}4b}".format((insts[i] >> 12) & 0xf, fill="0"))
      print("src: {0:{fill}4b}".format((insts[i] >> 16) & 0xf, fill="0"))
      print("opnd2: {0:{fill}12b}".format(insts[i] & 0xfff, fill="0"))

    b_inst = insts[3]
    print("============", bin(b_inst))
    print("condition: {0:{fill}4b}".format((b_inst >> 28) & 0xf, fill="0"))
    print("link: {0:{fill}1b}".format((b_inst >> 24) & 0x1, fill="0"))
    print("offset: {0:{fill}24b}".format(b_inst & 0xffffff, fill="0"))
