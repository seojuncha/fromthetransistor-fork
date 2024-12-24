#!/usr/bin/env python3

from as_parser import Parser
from as_encoder import Encoder

if __name__ == "__main__":
  p = Parser("sample/from-chatgpt/data_processing.s")
  objs = p.parse()

  for o in objs:
    o.dump()

  e = Encoder()
  e.encode(objs)
