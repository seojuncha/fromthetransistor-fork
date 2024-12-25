module cpu (
  input clk,
  input n_reset,
  input n_rw,  // LOW in read
  input [31:0] instrunction,
  input [31:0] address_bus,
  input [31:0] data_in_bus,
  output reg [31:0] data_out_bus
);
  /**
   * r0-r13: general purpose register
   * r14 : link register (LR)
   * r15 : program counter (PC)
   */
  reg [31:0] register [0:15];
  reg [31:0] pc;

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
  reg [31:0] cpsr;

  initial begin
    pc = 0x00000000
  end

  always @(posedge clk or negedge reset) begin
    if (!reset) begin

    end else begin

    end
  end

endmodule
