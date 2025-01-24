module sram (
  input clk,
  input rst,
  input en,
  input rd_en,   // read enable
  input wr_en,   // write enable
  input [15:0] addr,
  input [31:0] idata,
  output reg [31:0] odata
);
  reg [31:0] memory [0:16383];

  // test fetch process only
  // explicitely copy a binary file into a current working directory.
  // initial begin
  //   $readmemb("little-mov.bin", memory);
  // end

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      integer i;
      for (i = 0; i < 16383; i=i+1) begin
        memory[i] = 32'd0;
      end
      odata <= 0;
    end else begin
      if (en) begin
        if (wr_en)
          memory[addr] <= idata;
        if (rd_en)
          odata <= memory[addr];
      end
    end
  end

endmodule
