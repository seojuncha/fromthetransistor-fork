module uart #(parameter CLOCK_F = 100000000, 
              parameter BAUDRATE = 9600, 
              parameter FIFO_DEPTH = 2)
  (
  // default
  input clk,
  input reset,
  // serial wire
  input rx,
  output tx,
  // data channel
  input [7:0] data_in,
  output reg [7:0] data_out,
  // control
  input rx_start,
  input tx_start, output reg rx_enable, output reg tx_enable
); 
  // FIXME: It isn't worked!!
  // real BAUD_TICK = (1 / BAUDRATE) / (1 / CLOCK_F);

//  baudrate ?  bits per sec
//   9600bits/sec
// clock = f, baudrate = b
// baudrate period (BT)
//  1 bit = 1 / 9600 sec = 0.0001041666 sec = 0.1041666 ms = 104.166 us = 104166 ns
//  T_baud = 1 / BAUDRATE

// clock period (t)
//  Clock Frequency (How many clocks in one second)
//  Frequency (F) = the number of clocks(N) / one sec
//  one clock period (T_clk)
//  one sec = T_clk * N
//  T_clk = one sec / N
//  so that T_clk = 1 / F

//  How many clocks are needed to transmit 1 bit?
// How many clocks = M
// one clock period = P = 1 / F
// M * P = total clock count in one second.
// Time to transmit(or receive) one bit = 1 / B(audrate)
// So, M * P(sec) = (1 / B)(sec)
//  M = (1 / B) / P = (1 / B) / (1 / F)
// increate 'count' to generate when the clock cycle reaches to target clock cycle.
// for instance, 100 MHz, 9600 Baudrate
// Rquired clcok cycles = (1/9600) / (1/100_000_000) = 10416
// 0 <= M <= 16384(2**14)
  reg [13:0] baud_tick_counter;

// shift register
  reg [9:0] shift_reg;

// two fifo buffers, (rx and tx)
  integer i;
  reg [7:0] input_fifo[FIFO_DEPTH];
  reg [7:0] output_fifo[FIFO_DEPTH];

// data frame: 10-bit
//   star bit(1), data bit(8), stop bit(1)

  initial begin
    $display("temp1: %f", 1 / BAUDRATE);
    $display("clock freq: %d", CLOCK_F);
    $display("baudrate: %d", BAUDRATE);
    // $display("baudtick: %f", BAUD_TICK);
    baud_tick_counter <= 0;
    rx_enable <= 0;
    tx_enable <= 0;
    data_out <= 8;
    shift_reg <= 10'b0;
    for (i = 0; i < FIFO_DEPTH; i = i + 1) begin
      input_fifo[i] = 10'b0;
      output_fifo[i] = 10'b0;
    end
  end

  // TODO: rx_enable or tx_enable is set by the status of fifo buffer.

  always @(posedge clk or negedge reset) begin
    if (reset) begin
      rx_enable <= 0;
      tx_enable <= 0;
      data_out <= 8;
      shift_reg <= 10'b0;
      for (i = 0; i < FIFO_DEPTH; i = i + 1) begin
        input_fifo[i] = 10'b0;
        output_fifo[i] = 10'b0;
      end
    end else if (rx_start) begin

    end else if (tx_start) begin

    end
  end

endmodule
