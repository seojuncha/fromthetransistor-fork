module top(
  input clk,
  input rst
);
  wire input_a;
  // reg input_a;
  reg input_b;
  wire result_c;
  wire result_not_a;

  and_child and_child_inst(
    .a(input_a),
    .b(input_b),
    .c_reg(result_c)
  );

  not_child not_child_inst(
    .a(input_a),
    .not_a(result_not_a)
  );

  assign input_a = 1'b1;

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      // input_a <= 1;
      input_b <= 1;
    end else begin
      $display("[%t] %b -> %b", $time, result_c, result_not_a);
    end
  end

endmodule
