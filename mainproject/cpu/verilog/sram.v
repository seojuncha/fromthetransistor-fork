module sram (
  input clk,
  input wr,   // 1: write, 0: read
  input [17:0] address,
  input [31:0] data_in,
  output reg [31:0] data_out,
  output reg ready
);
  reg [31:0] memory [0:4];

  always @(posedge clk) begin
    if (wr)
      data_out <= memory[address];
  end
  assign memory[address] = data_in;

endmodule
