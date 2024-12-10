#!/bin/bash

if [ -d gtkwave ]; then
  echo "gtkwave is already downloaded.."
  exit 1
fi 

git clone https://github.com/gtkwave/gtkwave.git

if [ ! -d gtkwave ]; then
  echo "Failed to clone gtkwave"
  exit 2
fi

brew install desktop-file-utils shared-mime-info       \
             gobject-introspection gtk-mac-integration   \
             meson ninja pkg-config gtk+3 gtk4

cd gtkwave
meson setup build
meson compile -C build

## Ouput: build/src/gtkwave
## Run: ./gtkwave