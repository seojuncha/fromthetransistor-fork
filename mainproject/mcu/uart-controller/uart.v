module uart (
  input clk,
  input rst,
  input rx,    // external
  output reg [7:0] rx_data_out,  // rx -> sfhit_reg -> data_out

  input tx_start,
  output tx,   // external
  output tx_ready,
  output tx_busy,
  input [7:0] tx_data_in,  // data_in -> shift_reg -> tx

  input [13:0] baud_tick_max
); 
  localparam IDLE = 2'b00,
            TX_START = 2'b01,
            TX_DATA = 2'b10,
            RX_DATA = 2'b11;

  reg [1:0] state, next_state;

  reg [13:0] baud_tick_counter;

  reg [3:0] bit_count;
  reg [7:0] rx_shift_reg;
  reg [8:0] tx_shift_reg;

  assign tx = tx_shift_reg[0];
  assign tx_ready = (state == TX_START);
  assign tx_busy = (state == TX_DATA);

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      baud_tick_counter <= 0;
      rx_data_out <= 0;
      bit_count <= 0;
      rx_shift_reg <= 0;
      tx_shift_reg <= 9'b111111111;
    end else begin
      case(state)
        IDLE: begin
          bit_count <= 0;
          rx_shift_reg <= 0;
          tx_shift_reg <= 9'b111111111;
          baud_tick_counter <= 0;
        end
        TX_START: begin
          tx_shift_reg <= {tx_data_in, 1'b0};
        end
        TX_DATA: begin
          baud_tick_counter <= baud_tick_counter + 1;
          if (baud_tick_counter == baud_tick_max) begin
            $display("bitcount: %d tx : %d",bit_count, tx);
            baud_tick_counter <= 0;
            bit_count <= bit_count + 1;
            tx_shift_reg <= {1'b1, {tx_shift_reg[8:1]}};
          end
        end
        RX_DATA: begin
          baud_tick_counter <= baud_tick_counter + 1;
          if (baud_tick_counter == baud_tick_max) begin
            $display("bitcount: %d rx : %d",bit_count, rx);
            bit_count <= bit_count + 1;
            // from lsb to msb
            rx_shift_reg <= {rx, rx_shift_reg[7:1]};
            baud_tick_counter <= 0;
            if (bit_count == 8) begin
              $display("match!");
              rx_data_out <= rx_shift_reg;
            end
          end
        end
      endcase
    end
  end

  // state control
  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      state <= IDLE;
    end else begin
      state <= next_state;
    end
  end

  always @(*) begin
    next_state = state;
    case(state)
      IDLE: begin
        $display("IDLE");
        if(tx_start)
          next_state = TX_START;
        if (rx == 0) begin
          next_state = RX_DATA;
        end
      end
      TX_START: begin
        $display("TX START");
        next_state = TX_DATA;
      end
      TX_DATA: begin
        if (bit_count > 9) begin
          $display("TX DONE");
          next_state = IDLE;
        end
      end
      RX_DATA: begin
        if (bit_count > 8) begin
          $display("RX DONE");
          next_state = IDLE;
        end
      end
    endcase
  end

endmodule
