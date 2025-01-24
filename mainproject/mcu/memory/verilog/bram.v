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

  // Only for debug
  initial begin
    $readmemb("bootrom.bin.txt", memory);
    $display("mem[0] 0x%08x mem[1] 0x%08x mem[2] 0x%08x mem[3] 0x%08x", memory[0], memory[1], memory[2], memory[3]);
  end

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      integer i;
      for (i = 0; i < 16383; i=i+1) begin
        memory[i] = 32'd0;
      end
      odata <= 32'd0;
    end else begin
      if (wr_en) begin
        $display("1111111111111: 0x%04x", addr);
        memory[addr] <= idata;
      end
      if (rd_en) begin
        $display("2222222222222: 0x%04x", addr);
        odata <= memory[addr];
      end
    end
  end

endmodule
