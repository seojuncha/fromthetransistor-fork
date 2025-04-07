## Main Project
This directory contains the core components of the system under construction. It represents the entire toolchain and MCU needed to build a minimal computer from scratch.

### Structure

- `assembler/` – Converts assembly source code to binary machine code.
- `compiler/` – (WIP) Translates high-level language to intermediate representation.
- `data/` – Stores assembly files, binary data, test programs, or input/output vectors.
- `linker/` – (next) Combines multiple object files into a final binary.
- `mcu/` – Hardware-level implementation of the system (includes bootloader, CPU, memory, UART, etc.).