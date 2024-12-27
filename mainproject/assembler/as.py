#!/usr/bin/env python3

from as_parser import Parser
from as_encoder import Encoder

if __name__ == "__main__":
  p = Parser("sample/from-claude/arm-test-data.s")
  objs = p.parse()
  e = Encoder()

  for o in objs:
    o.dump()
    addr, bits = e.encode(o)
    print(f"\t\t\t\t\t\t[0x{addr:x}] {bits:8x}")