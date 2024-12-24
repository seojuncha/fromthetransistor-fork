module cpu (
  input clk,
  input reset,
  input [31:0] instruction,
  input [31:0] address_bus,
);
  /**
   * r0-r13: general purpose register
   * r14 : link register (LR)
   * r15 : program counter (PC)
   */
  reg [14:0] register;
  reg pc;

  /** 
   * current program status register (CPSR)
   * +-31-+-30-+-29-+-28-+-27-+-26-----------8-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
   * | N  | Z  | C  | V  | Q  |      DNM       | I | F | T | M | M | M | M | M |
   * +----+----+----+----+----+----------------+---+---+---+---+---+---+---+---+
   * |            condition                    |             control           |
   * N (Negative)
   * Z (Zero)
   * C (Carry)
   * V (oVerflow)
   * T (Thumb) : should be zero
   * M[4:0] (Mode) : should be zero(user mode)
   */
  reg cpsr;

  always @(posedge clk or negedge reset) begin
    if (!reset) begin

    end else begin

    end
  end

endmodule
