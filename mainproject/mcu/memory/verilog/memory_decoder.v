module memory_decoder(
  input [31:0] addr,
  output reg bram_select,
  output reg sram_select,
  output reg flash_select,
  output reg peripheral_select
);
  always @(*) begin
    bram_select = 0;
    sram_select = 0;
    flash_select = 0;
    peripheral_select = 0;

    case (addr[31:16])
      16'h0000: bram_select = 1;
      16'h0001: sram_select = 1;
      16'h0002: flash_select = 1;
      16'h0003: peripheral_select = 1;
      default: begin
        bram_select = 0;
        sram_select = 0;
        flash_select = 0;
        peripheral_select = 0;
      end
    endcase
  end

endmodule
