import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer

@cocotb.test()
async def tb_uart(dut):
  # 100MHz frequency
  c = Clock(dut.clk, 10, units="ns")
  dut._log.info("Clock Frequency: %f", c.frequency)

  cocotb.start_soon(c.start())

  dut.baud_tick_max.value = int((1/9600) / (1/(c.frequency*1_000_000)))

  # Clock frequency & Baudrate test
  while True:
    await RisingEdge(dut.clk)

    if dut.rx_start.value != 1:
      await Timer(50, units="ns")
      dut.rx_start.value = 1

    if dut.rx_enable.value == 1:
      dut._log.info("rx_enable! baud_clk[max: %d]: %d", dut.baud_tick_max.value, dut.baud_tick_counter.value)
      break

