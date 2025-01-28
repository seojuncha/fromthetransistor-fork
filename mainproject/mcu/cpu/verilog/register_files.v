module register_files(
  input [3:0] read_addr,
  input [3:0] write_addr,
  output [31:0] read_data,
  input [31:0] write_data
);
  reg [31:0] register [0:15];

  /**
    * CPSR: Current Program Status Register
    * b[31]: Negative, b[30]: Zero, b[29]: Carry, b[28]: oVerflow
    * Other bits always SBZ
    */
  // reg [31:0] cpsr;

  initial begin
    $monitor("[%0t][REGFILE] read %2d / 0x%08x  write %2d / 0x%08x", $realtime, read_addr, read_data, write_addr, write_data);
  end

  assign read_data = register[read_addr];

  always @(*) begin
    $display("[%0t][REGFILE] update register [%2d] 0x%08h", $realtime, write_addr, write_data);
    register[write_addr] = write_data;
  end

endmodule
