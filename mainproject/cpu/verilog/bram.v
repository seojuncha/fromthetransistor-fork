module bram (
  input clk,
  input enable,
  input [17:0] address,
  output reg [31:0] data_out
);
  // 1MB size
  // reg [31:0] memory [0:262144];
  reg [31:0] memory [0:4];

  // test fetch process only
  // explicitely copy a binary file into a current working directory.
  initial begin
    // $readmemb("big-mov.bin", memory);
    $readmemb("little-mov.bin", memory);
  end

  always @(posedge clk) begin
    if (enable)
      data_out <= memory[address];
  end

endmodule
