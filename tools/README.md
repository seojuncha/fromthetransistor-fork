## cocotb

### Setup environment

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
