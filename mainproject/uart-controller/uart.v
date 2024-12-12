// ver 1. 
//  - Half Duplex
module uart (
  // default
  input clk,
  input reset,
  // serial wire
  input rx,
  output tx,
  // data channel
  input [7:0] data_in,         // to tx wire
  output reg [7:0] data_out,   // from rx wire
  // control
  input rx_start,
  input tx_start,
  output reg enable,   // 0: disabled, 1: enabled
  // config
  input [13:0] baud_tick_max
); 
  localparam IDLE = 2'b00, START = 2'b01, ACTIVE = 2'b10, STOP = 2'b11;

  // IDLE: enable=1
  // START: enable=0, 
  // ACTIVE: increate baud tick,
  // STOP: enable=1, send stop bit
  reg [1:0] state;

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

  reg [3:0] shift_index;
  reg [9:0] shift_reg;

  integer i;
  // reg [7:0] fifo[FIFO_DEPTH];

// data frame: 10-bit
//   star bit(1), data bit(8), stop bit(1)

  initial begin
    state = IDLE;
  end

  always @(posedge clk or negedge reset) begin
    if (reset) begin
      state <= IDLE;
    end else if (rx_start || tx_start) begin
      state <= START;
      baud_tick_counter <= baud_tick_counter + 1;
    end
  end

  always @(*) begin
    case(state)
      IDLE: begin
        baud_tick_counter <= 0;
        enable <= 1;
        data_out <= 0;
        shift_reg <= 0;
      end
      START: begin
        state <= ACTIVE;
        if (rx_start) begin
          shift_reg <= {1'b0, data_in, 1'b0};
        end else if (tx_start) begin

        end
      end
      ACTIVE: begin
        // shift and send/receive one bit
        if (baud_tick_counter >= baud_tick_max) begin
          state <= ACTIVE;
          // baud_tick_counter <= 14'b0;
        end
      end
      STOP: begin
        state <= IDLE;
      end
    endcase
  end

  // assign tx = ??
  // assign rx = ??

endmodule
