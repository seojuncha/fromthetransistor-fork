#!/bin/bash

verilator --binary -j `nproc` -Wall $1.v