import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer

import binascii

@cocotb.test()
async def tb_uart(dut):
  # 100MHz frequency
  c = Clock(dut.clk, 10, units="ns")
  dut._log.info("Clock Frequency: %f", c.frequency)

  cocotb.start_soon(c.start())

  baud_tick = int((1/9600) / (1/(c.frequency*1_000_000)))
  dut.baud_tick_max.value = baud_tick

  # Character to send
  char = 'b'  # 8-bit
  ascii_value = ord(char)  # ASCII value of 'a'
  binary_string = format(ascii_value, '08b')  # Convert to 8-bit binary string
  
  print(f"Sending character '{char}' as binary: {binary_string}")

  # Wait for 50 sec to start
  await Timer(50, units="ns")

  for i in range(10):
    if i == 0:
      input = 0
    elif i == 9:
      input = 1
    else:
      input = binary_string[i-1]
    dut.rx.value = int(input)
    await Timer(104166, units="ns")
    dut._log.info("e %d / send %d rx %s / i %d / reg %s", dut.enable, int(input), dut.rx.value, dut.shift_index.value, dut.shift_reg.value.binstr)

  await RisingEdge(dut.clk)
  char = binascii.unhexlify("%x" % int("0b"+dut.shift_reg.value.binstr[::-1][1:9], 2))
  dut._log.info("final result: %s [%s] ", char, dut.shift_reg.value.binstr)
