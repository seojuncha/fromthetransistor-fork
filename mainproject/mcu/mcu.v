module mcu(
  input clk,
  input n_reset,
);
  wire [31:0] address_bus;
  wire [31:0] data_in_bus;
  wire [31:0] data_out_bus;

  wire memory_write;
  wire memory_read;

  reg flash_erase;
  wire flash_busy;

  wire [31:0] bram_data_out;
  wire [31:0] sram_data_out;
  wire [31:0] flash_data_out;
  wire [31:0] mmio_data_out;

  cpu cpu_inst(
    .clk(clk),
    .n_reset(n_reset),
    .address(address_bus)
    .data_in(data_in_bus),
    .data_out(data_out_bus),
    .memory_read(memory_read),
    .memory_write(memory_write),
  );

  // memory
  bram bram_inst(
    .clk(clk),
    .rst(n_reset),
    .rd_en(memory_read),
    .wr_en(memory_write),
    .address(address_bus[15:0]),
    .data_in(data_out_bus),
    .data_out(bram_data_out)
  );

  sram sram_inst(
    .clk(clk),
    .rst(n_reset),
    .rd_en(memory_read),
    .wr_en(memory_write),
    .address(address_bus[15:0]),
    .data_in(data_out_bus),
    .data_out(sram_data_out)
  );

  flash flash_inst(
    .clk(clk)
    .rd_en(memory_read),
    .wr_en(memory_write),
    .erase_en(flash_erase),
    .addr(address_bus[11:0]),
    .data_in(data_out_bus),
    .data_out(flash_data_out),
    .busy(flash_busy)
  );

  // peripheral
  // TODO: need a peripheral controller.
  // uart uart_inst(
  //   .clk(clk),
  //   .rst(n_reset),
  //   .rx(),
  //   .rx_data_out(),
  //   .tx_start(),
  //   .tx(),
  //   .tx_ready(),
  //   .tx_busy(),
  //   .tx_data_in(),
  //   .baud_tick_max()
  // );

  always @(posedge clk) begin
    if (!n_reset) begin
      
    end else begin
    end
  end

  always @(*) begin
    case (address_bus[31:16])
      16'h0000: data_in_bus = bram_data_out;
      16'h0001: data_in_bus = sram_data_out;
      16'h0002: data_in_bus = flash_data_out;
      16'h0003: data_in_bus = mmio_data_out;
    endcase
  end

endmodule
