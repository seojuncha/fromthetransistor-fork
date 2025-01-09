import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer

import binascii

"""
  baudrate ?  bits per sec
   9600bits/sec
 clock = f, baudrate = b
 baudrate period (BT)
  1 bit = 1 / 9600 sec = 0.0001041666 sec = 0.1041666 ms = 104.166 us = 104166 ns
  T_baud = 1 / BAUDRATE
 clock period (t)
  Clock Frequency (How many clocks in one second)
  Frequency (F) = the number of clocks(N) / one sec
  one clock period (T_clk)
  one sec = T_clk * N
  T_clk = one sec / N
  so that T_clk = 1 / F
  How many clocks are needed to transmit 1 bit?
 How many clocks = M
 one clock period = P = 1 / F
 M * P = total clock count in one second.
 Time to transmit(or receive) one bit = 1 / B(audrate)
 So, M * P(sec) = (1 / B)(sec)
  M = (1 / B) / P = (1 / B) / (1 / F)
 increate 'count' to generate when the clock cycle reaches to target clock cycle.
 for instance, 100 MHz, 9600 Baudrate
 Rquired clcok cycles = (1/9600) / (1/100_000_000) = 10416
 0 <= M <= 16384(2**14)

 data frame: 10-bit
   star bit(1), data bit(8), stop bit(1)
"""

def char_to_bin(c):
  return format(ord(c), '08b')

@cocotb.test()
async def tb_uart(dut):
  # 100MHz frequency
  c = Clock(dut.clk, 10, units="ns")
  dut._log.info("Clock Frequency: %f", c.frequency)

  cocotb.start_soon(c.start())

  BAUDRATE = 115200
  onebit_ns = int((1/BAUDRATE) * 1000 * 1000 * 1000)
  clock_div = int((c.frequency*1_000_000) / (BAUDRATE * 16))
  dut._log.info("clock div: %d", clock_div)
  dut.baud_tick_max.value = clock_div

  dut.rst.value = 0
  await Timer(10, units="ns")
  dut.rst.value = 1
 
  while True:
    data = input("Enter the string(exit): ")
    if data == "exit":
      break
    print(f"input : {data}")

    for c in data:
      binchar = char_to_bin(c)
      dut._log.info("binchar: %s", binchar)
      for i in range(10):
        if i == 0:
          in_val = 0
        elif i == 9:
          in_val = 1
        else:
          in_val = binchar[i-1]
        dut.rx.value = int(in_val)
        for i in range(clock_div):
          await RisingEdge(dut.clk)
        dut._log.info("in: %d rx: %s b: %s rx_shift_reg: %s", int(in_val), dut.rx.value, dut.bit_count.value, dut.rx_shift_reg.value.binstr)
      await RisingEdge(dut.clk)
      dut._log.info("in: %d rx: %s b: %s data_out: %s", int(in_val), dut.rx.value, dut.bit_count.value, dut.rx_data_out.value.binstr)

      temp = dut.rx_data_out.value
      char = binascii.unhexlify("%x" % int("0b"+temp.binstr[::-1], 2))
      dut._log.info("final result: %s [%s] ", char, temp.binstr)

      # echo one character
      dut.tx_start.value = 1
      await RisingEdge(dut.tx_ready)
      dut.tx_data_in.value = temp

      binary_str = "0b"
      for i in range(10):
        if i > 1:
          print(dut.tx.value.binstr)
          binary_str = binary_str + dut.tx.value.binstr
        dut._log.info("busy: %s, out: %s, tx_shift_reg: %s", dut.tx_busy.value, dut.tx.value, dut.tx_shift_reg.value.binstr)
        for i in range(clock_div):
          await RisingEdge(dut.clk)

      dut._log.info("ECHO : %s", chr(int(binary_str, 2)))

      dut.tx_start.value = 0
      await RisingEdge(dut.clk)
      dut._log.info("END busy: %s, out: %s, tx_shift_reg: %s", dut.tx_busy.value, dut.tx.value, dut.tx_shift_reg.value.binstr)

      # Check data out bit
      for i in range(100):
          await RisingEdge(dut.clk)
      dut._log.info("FINAL CHECK busy: %s, out: %s, tx_shift_reg: %s", dut.tx_busy.value, dut.tx.value, dut.tx_shift_reg.value.binstr)
