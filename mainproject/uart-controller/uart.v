module uart (
  input clk,
  input reset,
  input rx,
  output reg enable,   // 0: disabled, 1: enabled

  input [13:0] baud_tick_max
); 
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

// data frame: 10-bit
//   star bit(1), data bit(8), stop bit(1)

  initial begin
    baud_tick_counter <= 0;
    enable <= 1;
    shift_reg <= 0;
    shift_index <= 0;
  end

  always @(posedge clk or negedge reset) begin
    if (reset) begin
      baud_tick_counter <= 0;
      enable <= 1;
      shift_reg <= 0;
    end else if (enable && !rx) begin    // start receiving
      enable <= 0;
      shift_index <= shift_index + 1;
      shift_reg[shift_index] <= rx;
    end else if (!enable) begin          // keep receiving
      baud_tick_counter <= baud_tick_counter + 1;
      if (baud_tick_counter >= baud_tick_max) begin
        baud_tick_counter <= 0;
        if (shift_index == 9) begin
          shift_reg[shift_index] <= rx;
          shift_index <= 0;
          enable <= 1;
        end else begin
          shift_index <= shift_index + 1;
          shift_reg[shift_index] <= rx;
        end
      end
    end
  end

endmodule
