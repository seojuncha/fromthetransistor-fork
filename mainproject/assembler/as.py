#!/usr/bin/env python3

"""
Usage:
as [--debug] asmfiles...

Options:
  --help       Print a help message

  --debug
    e.g) as --debug sample.s

  --verify
    e.g) as --verify sample.s

  --disassemble <binary_file>
    e.g) as --disassemble sample.bin
"""

import os, sys, argparse
from sys import platform
from pathlib import Path

from as_parser import Parser
from as_encoder import Encoder

def compare_binary_files(file1, file2):
    with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
        return f1.read() == f2.read()

if __name__ == "__main__":
  arg_parser = argparse.ArgumentParser(prog="as", usage="%(prog)s [--debug] [--verify] files...")
  arg_parser.add_argument("asmfiles", nargs="+", metavar="files", help="Input assembly files")
  arg_parser.add_argument("--debug", action="store_true", help="Print debug messages")
  arg_parser.add_argument("--verify", action="store_true", help="Compare with the original ARM assembler")

  args = arg_parser.parse_args()

  asm_path_list = []
  try:
    for file in args.asmfiles:
      asm_path_list.append(Path(file))

    invalid_extension_files = []
    [invalid_extension_files.append(asm_path) for asm_path in asm_path_list if asm_path.suffix != ".s"]
    if len(invalid_extension_files):
      [print(f"Invalid file extension: {file}") for file in invalid_extension_files]
      sys.exit(1)
  except:
    print(f"Invalid assembly file name: {args.asmfiles}")
    sys.exit(1)

  if (args.verify):
    print("Verify option is enabled....")
    origin_dir = Path("gnu_out/")
    if not origin_dir.exists():
      origin_dir.mkdir(parents=True)

  p = Parser(debug=args.debug)
  e = Encoder(debug=args.debug)

  objs = [p.parse(asmfile) for asmfile in asm_path_list][0]
  [o.dump() for o in objs if args.debug == True]

  for asmpath in asm_path_list:
    asmname = asmpath.stem
    bin_path = asmpath.parent / (asmname + ".bin")
    with bin_path.open("wb") as f:
      for o in objs:
        addr, bits = e.encode(o)
        print(f"\t\t\t\t\t\t[0x{addr:x}] {bits:8x}")

        # 32-bit word alignment. store it by little endian.
        f.write(bits.to_bytes(length=4, byteorder="little"))

    if (args.verify):
      origin_obj_path = origin_dir / (asmname + ".o")
      origin_bin_path = origin_dir / (asmname + ".bin")
      if platform == "linux":
        print("Verifying in Linux....")
        os.system(f"arm-none-eabi-as {str(asmpath)} -o {str(origin_obj_path)}")
        os.system(f"arm-none-eabi-objcopy -O binary {str(origin_obj_path)} {str(origin_bin_path)}")
      elif platform == "darwin":
        print("Verifying in MacOS....")
        os.system(f"clang -target armv4t-none-eabi -c -o {str(origin_obj_path)} {str(asmpath)}")
        os.system(f"llvm-objcopy --input-target=elf32-littlearm --output-target=binary {str(origin_obj_path)} {str(origin_bin_path)}")
      else:
        print("NOT supported OS: ", platform)
        exit(1)

      if compare_binary_files(origin_bin_path, bin_path):
        print("Verified!!")
      else:
        print("NOT Verified....")
        os.system(f"cmp -l {str(origin_bin_path)} {str(bin_path)}")
