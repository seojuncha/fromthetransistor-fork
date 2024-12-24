from as_obj import InstructionObj

class Parser:
  def __init__(self, filepath=None):
    self._file = open(filepath, "r")
    self._not_support_count = 0

  def parse(self) -> list[InstructionObj]:
    objs = []
    for i, line in enumerate(self._file.readlines()):
      ret, line = Parser.cleaner(line)
      if ret: continue

      line = Parser.remove_line_comment(line)
      if Parser.is_directives(line):
        pass
      elif Parser.is_label(line):
        print(f"not support yet:\t{line}")
        self._not_support_count += 1
      else:
        objs.append(InstructionObj.create(i+1, line))

    self._file.close()

    return objs

  @staticmethod
  def cleaner(line: str):
    return ((line.isspace() or len(line) == 0 or line.startswith(("@",";"))), line.rstrip())
  
  @staticmethod
  def is_directives(line: str):
    return line.startswith(".")
  
  @staticmethod
  def is_label(line: str):
    return line.endswith(":")
  
  @staticmethod
  def remove_line_comment(line: str):
    return line.split("@")[0].strip()
