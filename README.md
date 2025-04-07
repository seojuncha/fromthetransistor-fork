## From the Transistor to Web Browser (My Edition)
This project is a fork of the "From the Transistor" initiative. It aims to build a minimal computer system starting from low-level components like an assembler and linker, all the way to an MCU implementation.

> For Gorge Hotz's original course overview, refer to the [README.md](./README.origin.md)

## Project Structure
```
fromthetransistor-fork/
  ├── docs/ # Project documentation and technical references
  ├── mainproject/     # Core system components
  │     ├── assembler/ # Converts assembly code into machine code
  │     ├── compiler/  # High-level to intermediate code translator (WIP)
  │     ├── data/      # Binary data and test vectors
  │     ├── linker/    # Combines multiple object files into a final binary
  │     └── mcu/       # Minimal Microcontroller Unit (MCU) implementation
  ├── subproject/      # Experimental or supplementary modules 
  ├── tools/           # Utility scripts and developer tools 
  ├── README.md        # Main project README (this file)
  ├── .gitignore       # Git ignore rules
  └── .gitmodules      # Git submodule(QEMU) references
```

## Getting Started

```shell
# Don't need to update submodules now.
$ git clone git@github.com:seojuncha/fromthetransistor-fork.git
```

## Goals
- Understand the full stack: from source code to physical logic

