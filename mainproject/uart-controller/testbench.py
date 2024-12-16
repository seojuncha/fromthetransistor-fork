import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer

import binascii

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
  baud_tick = int((1/BAUDRATE) / (1/(c.frequency*1_000_000)))
  dut.baud_tick_max.value = baud_tick

  while True:
    data = input("Enter the string(exit): ")
    if data == "exit":
      break
    print(f"input : {data}")
    for c in data:
      binchar = char_to_bin(c)
      for i in range(10):
        if i == 0:
          in_val = 0
        elif i == 9:
          in_val = 1
        else:
          in_val = binchar[i-1]
        dut.rx.value = int(in_val)
        await Timer(onebit_ns, units="ns")
        dut._log.info("e %d / send %d rx %s / i %d / reg %s", dut.enable, int(in_val), dut.rx.value, dut.shift_index.value, dut.shift_reg.value.binstr)

      await RisingEdge(dut.clk)
      char = binascii.unhexlify("%x" % int("0b"+dut.shift_reg.value.binstr[::-1][1:9], 2))
      dut._log.info("final result: %s [%s] ", char, dut.shift_reg.value.binstr)