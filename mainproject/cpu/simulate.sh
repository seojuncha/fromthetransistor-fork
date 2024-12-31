#!/bin/bash

make clean
WAVE=1 TOPLEVEL=$1 MODULE=tb_$1 make