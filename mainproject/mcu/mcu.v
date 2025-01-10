module mcu(
  input clk,
  input n_reset,
);
  wire [31:0] address_bus;
  wire [31:0] data_bus;

  cpu cpu_inst(
    .clk(clk),
    .n_reset(n_reset),
    .address(address_bus)
    .data_in(),
    .data_out(),
  );

  uart uart_inst(
    .clk(clk),
    .rst(n_rest),
    .rx(),
    .rx_data_out(),
    .tx_start(),
    .tx(),
    .tx_ready(),
    .tx_busy(),
    .tx_data_in(),
    .baud_tick_max()
  );

endmodule
