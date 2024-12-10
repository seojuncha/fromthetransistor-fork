import cocotb


@cocotb.test()
async def testbench(dut):
    dut._log.info("See below message!")
