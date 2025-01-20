module bram (
  input clk,
  input rst,
  input rd_en,   // read enable
  input wr_en,   // write enable
  input [15:0] address,
  input [31:0] data_in,    // from CPU
  output reg [31:0] data_out  // to CPU
);
  reg [31:0] memory [0:16383];

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      memory <= 0;
      data_out <= 0;
    end else begin
      if (wr_en)
        memory[address] <= data_in;
      if (rd_en)
        data_out <= memory[address];
    end
  end

endmodule
