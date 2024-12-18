#!/bin/sh

verilator --cc --exe --build -j 8 -Wall uart_tb.cc uart.v