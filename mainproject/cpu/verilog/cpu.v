module cpu (
  input clk,
  input n_reset
);
  localparam IDLE = 2'b00, FETCH = 2'b01, DECODE = 2'b10, EXECUTE = 2'b11;
  reg [1:0] state;
  reg [1:0] next_state;

  // registers
  reg [31:0] register [0:12];
  reg [31:0] sp;  // R13
  reg [31:0] lr;  // R14
  reg [31:0] pc;  // R15

  // interal registers to communicate with ALU
  reg [31:0] alu_a;   // operand 1
  reg [31:0] alu_b;   // operand 2(shifter operand)
  reg [31:0] alu_out;  // result
  reg carry_in;

  // barrel shifter
  reg [31:0] shift_value;
  reg [4:0] shift_amt;
  reg [1:0] shift_type;
  reg shifter_carry_out;

  // b[31]: Negative, b[30]: Zero, b[29]: Carry, b[28]: oVerflow
  // Other bits always SBZ
  reg [31:0] cpsr;

  // instruction register
  reg [31:0] ir;

  // interfaces
  wire [31:0] address;
  wire [31:0] data_in;
  wire [31:0] data_out;

  // internal wires
  reg bram_enable;
  wire [17:0] bram_addr;

  reg little_endian;

  assign bram_addr = pc >> 2;

  // modules
  bram bram_inst (
    .clk(clk),
    .enable(bram_enable),
    .address(bram_addr),
    .data_out(data_in)
  );

  barrel_shifter barrel_shifter_inst(
    .shift_in(shift_value),
    .shift_type(shift_type),
    .shift_imm(shift_amt),
    .rs(),
    .is_imm_32(),
    .is_use_rs(),
    .cary_in(carry_in),
    .shifter_operand(alu_b),
    .shift_carry_out(shifter_carry_out)
  );

  alu alu_inst (
    .opcode(ir[24:21]),
    .operand1(alu_a),
    .operand2(alu_b),
    .carry_in(carry_in),
    .result(alu_out),
    .negative_flag(cpsr[31]),
    .zero_flag(cpsr[30]),
    .carry_out_flag(cpsr[29])
  );

  // only for debug
  initial begin
    $monitor("[%0t] PC [%x] INST [0x%8x]",$time, pc, ir);
  end

  always @(posedge clk) begin
    case (state)
      // skip condition flag
      DECODE: begin
        // data processing
        if (ir[27:26] == 2'b00) begin
          alu_a <= register[ir[19:16]];
          // immediate, rotate_imm with imm_8
          if (ir[25] == 1'b1) begin
            
          end
        end
      end
      EXECUTE: begin
        register[ir[15:12]] <= alu_out;
      end
    endcase
    
  end

  always @(posedge clk or negedge n_reset) begin
    if (!n_reset) begin
      state <= IDLE;
    end else begin
      state <= next_state;
    end
  end

  always @(*) begin
    next_state = state;
    case (state)
      IDLE: begin
        sp = 32'd0;
        lr = 32'd0;
        pc = 32'd0;
        next_state = IDLE;
        if (bram_enable)
          next_state = FETCH;
        $display("IDLE");
      end
      FETCH: begin
        if (little_endian)  // for convinent, little->big
          ir = (data_in[7:0] << 24) | (data_in[15:8] << 16) | (data_in[23:16] << 8) | (data_in[31:24]);
        else
          ir = data_in;
        pc = pc + 4;
        next_state = DECODE;
        $display("FETCH");
      end
      DECODE: begin
        next_state = EXECUTE;
        $display("DECODE");
      end
      EXECUTE: begin
        next_state = FETCH;
        $display("EXECUTE");
      end
    endcase
  end



endmodule
