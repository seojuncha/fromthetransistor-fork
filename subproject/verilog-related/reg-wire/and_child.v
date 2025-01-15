module and_child (
  input a,
  input b,
  // output c_wire,
  output reg c_reg
);
  // assign c_wire = a & b;

  always @(*)
    c_reg = a & b;

endmodule
