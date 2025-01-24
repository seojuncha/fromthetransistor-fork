module memory_decoder(
  input [31:0] addr,
  output reg bram_select,
  output reg sram_select,
  output reg flash_select,
  output reg peripheral_select,
  output reg error
);
  always @(*) begin
    bram_select = 0;
    sram_select = 0;
    flash_select = 0;
    peripheral_select = 0;
    error = 0;
    $display("[MEM][DEC] address: 0x%08x", addr);
    case (addr[31:16])
      16'h0000: bram_select = 1;
      16'h0001: sram_select = 1;
      16'h0002: flash_select = 1;
      16'h0003: peripheral_select = 1;
      default: begin
        $display("[MEM][DEC] error: invalid address range");
        bram_select = 0;
        sram_select = 0;
        flash_select = 0;
        peripheral_select = 0;
        error = 1;
      end
    endcase
  end

endmodule
