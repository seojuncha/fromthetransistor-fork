module flash(
  input clk,
  input rd_en,
  input wr_en,
  input erase_en,
  input [11:0] addr,
  input [31:0] data_in,
  output reg[31:0] data_out,
  output reg busy
);
  reg [31:0] memory [0:1023];

  always @(posedge clk) begin
    
  end

endmodule
