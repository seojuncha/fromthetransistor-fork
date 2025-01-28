module bram (
  input clk,
  input rst,
  input rd_en,
  input wr_en,
  input [15:0] addr,
  input [31:0] idata,
  output reg [31:0] odata
);
  reg [31:0] memory [0:16383];
  wire [13:0] word_addr;   // log2(16384)
  assign word_addr = addr[15:2];

  // initial begin
  //   odata = 32'd0;
  // end

  always @(posedge clk or negedge rst) begin
    // Only for debug
    if (!rst) begin
      // $readmemb("bootrom.bin.txt", memory);
      // $display("mem[0] 0x%08x mem[1] 0x%08x mem[2] 0x%08x mem[3] 0x%08x", memory[0], memory[1], memory[2], memory[3]);

      integer i;
      for (i = 0; i < 16383; i=i+1) begin
        memory[i] = 32'd0;
      end
      odata <= 32'd0;
    end else begin
      if (wr_en) begin
        memory[word_addr] <= idata;
      end
      if (rd_en) begin
        $display("BRAM read[0x%0d]: 0x%08x",word_addr, memory[word_addr]);
        odata <= memory[word_addr];
      end
    end
  end

endmodule
