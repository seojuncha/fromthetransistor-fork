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
  reg [31:0] bram_idata, sram_idata, flash_idata, peripheral_idata;
  wire flash_error;

  memory_decoder memdec(
    .addr(addr),
    .bram_select(bram_selected),
    .sram_select(sram_selected),
    .flash_select(flash_selected),
    .peripheral_select(peripheral_selected)
  );

  bram bram_mem(
    .clk(clk),
    .rst(rst),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[15:0]),
    .idata(bram_idata),
    .odata(bram_odata)
  );

  sram sram_mem(
    .clk(clk),
    .rst(rst),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[15:0]),
    .idata(sram_idata),
    .odata(sram_odata)
  );


  flash flash_mem(
    .clk(clk),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .erase_en(flash_erase),
    .addr(addr[11:0]),
    .idata(flash_idata),
    .odata(flash_odata),
    .busy(flash_busy),
    .error(flash_error)
  );
  assign flash_error = error;

  peripheral peripheral_mem(
    .clk(clk),
    .rd_en(cpu_read_mem),
    .wr_en(cpu_write_mem),
    .addr(addr[11:0]),
    .idata(peripheral_idata),
    .odata(peripheral_odata)
  );

  always @(*) begin
    error = 0;
    if (cpu_read_mem) begin
      if (bram_selected) odata_to_cpu = bram_odata;
      else if (sram_selected) odata_to_cpu = sram_odata;
      else if (flash_selected) odata_to_cpu = flash_odata;
      else if (peripheral_selected) odata_to_cpu = peripheral_odata;
      else error = 1;
    end else if (cpu_write_mem) begin
      if (bram_selected) bram_idata = idata_from_cpu;
      else if (sram_selected) sram_idata = idata_from_cpu;
      else if (flash_selected) flash_idata = idata_from_cpu;
      else if (peripheral_selected) peripheral_idata = idata_from_cpu;
      else error = 1;
    end else begin
      error = 1;
    end
  end

endmodule
