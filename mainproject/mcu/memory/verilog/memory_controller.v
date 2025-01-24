module memory_controller(
  input clk,
  input rst,
  input cpu_read_mem,
  input cpu_write_mem,
  input [31:0] addr,
  input [31:0] idata_from_cpu,
  output reg [31:0] odata_to_cpu,
  output reg error
);
  wire bram_selected, sram_selected, flash_selected, peripheral_selected;
  wire [31:0] bram_odata, sram_odata, flash_odata, peripheral_odata;
  wire decode_error, flash_error;

  memory_decoder memdec(
    .addr(addr),
    .bram_select(bram_selected),
    .sram_select(sram_selected),
    .flash_select(flash_selected),
    .peripheral_select(peripheral_selected),
    .error(decode_error)
  );

  bram bram_mem(
    .clk(clk),
    .rst(rst),
    .en(bram_selected),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[15:0]),
    .idata(idata_from_cpu),
    .odata(bram_odata)
  );

  sram sram_mem(
    .clk(clk),
    .rst(rst),
    .en(sram_selected),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[15:0]),
    .idata(idata_from_cpu),
    .odata(sram_odata)
  );


  flash flash_mem(
    .clk(clk),
    .en(flash_selected),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .erase_en(flash_erase),
    .addr(addr[11:0]),
    .idata(idata_from_cpu),
    .odata(flash_odata),
    .busy(flash_busy),
    .error(flash_error)
  );

  peripheral peripheral_mem(
    .clk(clk),
    .en(peripheral_selected),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[11:0]),
    .idata(idata_from_cpu),
    .odata(peripheral_odata)
  );

  always @(*) begin
    error = 0;
    if (decode_error) error = 1;
    else if (flash_error) error = 1;
    else begin
      $display("[MEM][CTL] bram[%b] sram[%b] flash[%b] peripheral[%b]", bram_selected, sram_selected, flash_selected, peripheral_selected);
      if (cpu_read_mem) begin
        if (bram_selected) odata_to_cpu = bram_odata;
        else if (sram_selected) odata_to_cpu = sram_odata;
        else if (flash_selected) odata_to_cpu = flash_odata;
        else if (peripheral_selected) odata_to_cpu = peripheral_odata;
        else error = 1;
        $display("[MEM][CTL] read: 0x%08x", odata_to_cpu);
      end 
    end
  end

endmodule
