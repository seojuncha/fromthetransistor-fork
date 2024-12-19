module fsm (
  input clk,
  input rst,
  input start,
  input data_in,
  output reg done,
  output reg [7:0] data_out
);
  localparam IDLE = 2'b00, START = 2'b01, PROCESS = 2'b10, DONE = 2'b11;
  reg [1:0] state, next_state;
  reg [2:0] data_index;

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      done <= 0;
      data_out <= 0;
      data_index <= 0;
    end else begin
      case (state)
        IDLE: begin
          done <= 0;
        end
        START: begin
          data_index <= data_index + 1;
          data_out[data_index] <= data_in;
        end
        PROCESS: begin
          data_index <= data_index + 1;
          data_out[data_index] <= data_in;
        end
        DONE: begin
          done <= 1;
        end
      endcase
    end
  end

  // state transition
  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      state <= IDLE;
    end else begin
      state <= next_state;
    end
  end

  // state change
  always @(*) begin
    next_state = state;
    case (state)
      IDLE: begin
        $display("IDLE");
        if (start) begin
          next_state = START;
        end
      end
      START: begin
        $display("START");
        next_state = PROCESS;
      end
      PROCESS: begin
        $display("PROCESS");
        if (data_index == 7)
          next_state = DONE;
      end
      DONE: begin
        $display("DONE");
        next_state = IDLE;
      end
    endcase
  end

endmodule
