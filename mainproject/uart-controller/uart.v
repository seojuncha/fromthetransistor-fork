module uart (
  input clk,
  input rst,
  input rx,    // external
  output reg [7:0] rx_data_out,  // rx -> sfhit_reg -> data_out

  input tx_start,
  output tx,   // external
  output tx_ready,
  input [7:0] tx_data_in,  // data_in -> shift_reg -> tx

  input [13:0] baud_tick_max
); 
  localparam IDLE = 2'b00,
            TX_DATA = 2'b01,
            RX_DATA = 2'b10;

  reg [1:0] state, next_state;

  reg [13:0] baud_tick_counter;

  reg [3:0] bit_count;
  reg [7:0] shift_reg;

  assign tx = shift_reg[7];

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      baud_tick_counter <= 0;
      rx_data_out <= 0;
      bit_count <= 0;
      shift_reg <= 0;
    end else begin
      case(state)
        IDLE: begin
          bit_count <= 0;
          shift_reg <= 0;
          baud_tick_counter <= 0;
        end
        TX_DATA: begin
          
        end
        RX_DATA: begin
          baud_tick_counter <= baud_tick_counter + 1;
          if (baud_tick_counter == baud_tick_max) begin
            $display("bitcount: %d rx : %d",bit_count, rx);
            bit_count <= bit_count + 1;
            shift_reg <= {rx, shift_reg[7:1]};
            baud_tick_counter <= 0;
            if (bit_count == 8) begin
              $display("match!");
              rx_data_out <= shift_reg;
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
        if(tx_start)
          next_state = TX_DATA;
        if (rx == 0) begin
          next_state = RX_DATA;
        end
      end
      TX_DATA: begin

      end
      RX_DATA: begin
        if (bit_count > 8) begin
          $display("DONE");
          next_state = IDLE;
        end
      end
    endcase
  end

endmodule
