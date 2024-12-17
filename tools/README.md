# Setup environment

__MacOS__
- Processor: Apple Sillicon M2
- version: Sequoia 15.1.1

```shell
$ brew install icarus-verilog
$ brew install cocotb
```

Confirm cocotb installation
```shell
$ cocotb-config
$ cocotb-config --python-bin
```

Confirm icarus-verilog
```shell
$ iverilog
```

Build gtkwave
```shell
$ git clone https://github.com/gtkwave/gtkwave.git
$ brew install desktop-file-utils shared-mime-info       \
             gobject-introspection gtk-mac-integration   \
             meson ninja pkg-config gtk+3 gtk4
$ cd gtkwave
$ meson setup build
$ meson compile -C build
$ cd build/src
$ ./gtkwave
```

Build qemu for arm only
```shell
$ git clone -b v9.2.0 https://gitlab.com/qemu-project/qemu.git
$ cd qemu
$ mkdir build
$ cd build
$ ../configure --target-list=arm-linux-user,arm-softmmu
$ make && make install
```
```shell
$ qemu-arm --version
$ qemu-system-arm --version
```

Install Cross-Compiler (Ubuntu)
```shell
$ sudo apt install gcc-arm-none-eabi gdb-multiarch
```
