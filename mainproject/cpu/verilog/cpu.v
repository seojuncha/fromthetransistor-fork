module cpu (
  input clk,
  input n_reset
);
  localparam IDLE = 3'b000, 
             FETCH = 3'b001, 
             DECODE = 3'b010, 
             EXECUTE = 3'b011,
             WRITE_BACK = 3'b100,
             DONE = 3'b101;

  localparam DATA_PROCESSING_REG = 3'b000,
             DATA_PROCESSING_IMM = 3'b001,
             LOAD_STORE_IMM = 3'b010,
             LOAD_STORE_REG = 3'b011,
             BRANCH = 3'b101;

  reg [2:0] state;
  reg [2:0] next_state;

  // registers
  reg [31:0] register [0:12];
  reg [31:0] sp;  // R13
  reg [31:0] lr;  // R14
  reg [31:0] pc;  // R15

  // interal registers to communicate with ALU
  reg execute_enable;
  reg [31:0] alu_a;    // operand 1
  reg [31:0] alu_b;    // operand 2(shifter operand)
  reg [31:0] alu_out;  // result

  // barrel shifter
  reg [31:0] shift_value;
  reg [4:0] shift_amt;
  // reg [1:0] shift_type;
  // reg shifter_carry_out;

  // b[31]: Negative, b[30]: Zero, b[29]: Carry, b[28]: oVerflow
  // Other bits always SBZ
  reg [31:0] cpsr;
  wire neg_flag;
  wire zero_flag;
  wire carry_flag;
  wire overflow_flag;

  // instruction register
  reg [31:0] ir;

  // decoder interface
  reg decode_enable;
  reg decode_valid;
  reg [3:0] dec_opcode,
  reg [3:0] dec_rd,
  reg [3:0] dec_rn,
  reg [3:0] dec_rm,
  reg [1:0] dec_shift,
  reg [4:0] dec_shift_amount,
  reg dec_use_rs,
  reg dec_use_imm32,
  reg [3:0] dec_rs,
  reg [3:0] dec_rotate_imm,
  reg [7:0] dec_imm8,  
  reg dec_is_load,
  reg dec_is_unsigned_byte,
  reg dec_is_not_postindex,
  reg dec_is_added_offset,
  reg dec_is_write_back,
  reg [11:0] dec_offset_12,
  reg dec_branch_with_link,
  reg [23:0] dec_signed_immmed_24,
  reg dec_mem_read,
  reg dec_mem_write,

  // memory interfaces
  wire [31:0] address;
  wire [31:0] data_in;
  wire [31:0] data_out;
  wire mem_write;
  wire mem_ready;

  // internal wires
  reg bram_enable;
  wire [17:0] bram_addr;
  wire [17:0] sram_addr;

  reg little_endian;

  assign bram_addr = pc >> 2;

  assign neg_flag = cpsr[31];
  assign zero_flag = cpsr[30];
  assign carry_flag = cpsr[29];
  assign overflow_flag = cpsr[28];

  // modules
  bram bram_inst (
    .clk(clk),
    .enable(bram_enable),
    .address(bram_addr),
    .data_out(data_in)
  );

  sram sram_inst(
    .clk(clk),
    .wr(mem_write),
    .address(sram_addr),
    .data_in(data_out),
    .data_out(data_in),
    .ready(mem_ready)
  );

  decoder decoder_inst(
    .clk(clk),
    .enable(decode_enable),
    .instruction(ir),
    .opcode(dec_opcode),
    .rd(dec_rd),
    .rn(dec_rn),
    .rm(dec_rm),
    .shift(dec_shift),
    .shift_amount(dec_shift_amount),
    .use_rs(dec_use_rs),
    .use_imm32(dec_use_imm32),
    .rs(dec_rs),
    .rotate_imm(dec_rotate_imm),
    .imm8(dec_imm8),
    .is_load(dec_is_load),
    .is_unsigned_byte(dec_is_unsigned_byte),
    .is_not_postindex(dec_is_not_postindex),
    .is_added_offset(dec_is_added_offset),
    .is_write_back(dec_is_write_back),
    .offset_12(dec_offset_12),
    .branch_with_link(dec_branch_with_link),
    .signed_immmed_24(dec_signed_immmed_24),
    .mem_read(dec_mem_read),
    .mem_write(dec_mem_write),
    .valid(decode_valid)
  );

  barrel_shifter barrel_shifter_inst(
    .shift_in(shift_value),
    .shift_type(dec_shift),
    .shift_imm(shift_amt),
    .rs(dec_rs),
    .is_imm_32(dec_use_imm32),
    .is_use_rs(dec_use_rs),
    .cary_in(carry_flag),
    .shifter_operand(alu_b),
    .shift_carry_out(carry_flag)
  );

  alu alu_inst (
    .enable(execute_enable),
    .opcode(dec_opcode),
    .operand1(alu_a),
    .operand2(alu_b),
    .carry_in(carry_flag),
    .result(alu_out),
    .negative_flag(neg_flag),
    .zero_flag(zero_flag),
    .carry_out_flag(carry_flag)
  );

  // only for debug
  initial begin
    $monitor("[%0t] PC [%x] INST [0x%8x]",$time, pc, ir);
  end

  always @(posedge clk) begin
    case (state)
      IDLE: begin
      end

      FETCH: begin
        if (little_endian)  // for convinent, little->big
          ir <= (data_in[7:0] << 24) | (data_in[15:8] << 16) | (data_in[23:16] << 8) | (data_in[31:24]);
        else
          ir <= data_in;
        pc <= pc + 4;
      end

      DECODE: begin
        // TODO: error check
      end

      EXECUTE: begin
        case (dec_opcode)
          DATA_PROCESSING_REG: begin
            shift_value <= register[rm];
            shift_amt <= dec_shift_amount;
          end

          DATA_PROCESSING_IMM: begin
            shift_value <= dec_imm8;
            shift_amt <= dec_rotate_imm;
          end

          LOAD_STORE_IMM: begin
          end

          LOAD_STORE_REG: begin
          end

          BRANCH: begin
          end
        endcase
        alu_a <= register[rn];
      end

      WRITE_BACK: begin
        register[rd] <= alu_out;
      end

      DONE: begin
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
        next_state = IDLE;
        if (bram_enable)
          next_state = FETCH;
        $display("IDLE");
      end
      FETCH: begin
        next_state = DECODE;
        $display("FETCH");
      end
      DECODE: begin
        decode_enable = 1;
        if (decode_valid)
          next_state = EXECUTE;
        $display("DECODE");
      end
      EXECUTE: begin
        execute_enable = 1;
        next_state = WRITE_BACK
        // next_state = EXECUTE;
        $display("EXECUTE");
      end
      WRITE_BACK: begin
        next_state = DONE;
        $display("WRITE_BACK");
      end
      DONE: begin
        next_state = FETCH;
        $display("DONE");
      end
      default:
        next_state = IDLE;
    endcase
  end



endmodule
