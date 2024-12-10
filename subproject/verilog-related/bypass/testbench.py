import cocotb
from cocotb.triggers import Timer


@cocotb.test()
async def testbench(dut):
    dut._log.info("Start bypass testbench")
    # test_values =
    for value in [0, 1, 0, 1, 1, 0]:
        dut.input_signal.value = value
        await Timer(1, units="ns")
        dut._log.info("out: %s", dut.output_signal.value)
