module bypass_with_clk(
  input clk,
  input input_signal,
  output reg output_signal
);
  always @ (posedge clk) begin
    output_signal = input_signal;
  end

endmodule