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

  initial begin
    // Only for debug
    $readmemb("bootrom.bin", memory);
  end

  always @(posedge clk) begin
    if (erase_en) begin
      busy <= 1;
      memory <= 0;
    end else begin
      busy <= 0;
      if (rd_en) data_out <= memory[addr];
      if (wr_en) memory[addr] <= data_in;
    end
  end

endmodule
