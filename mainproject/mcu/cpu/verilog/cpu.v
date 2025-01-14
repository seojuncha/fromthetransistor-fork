module cpu (
  input clk,
  input n_reset,
  input little_endian_en,
  output reg [31:0] address,   // memory address
  input [31:0] data_in,        // from memory
  output reg[31:0] data_out,   // to memory
  output reg memory_read,
  output reg memory_write
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
  wire decode_valid;
  wire [3:0] dec_opcode;
  wire [3:0] dec_rd;
  wire [3:0] dec_rn;
  wire [3:0] dec_rm;
  wire [1:0] dec_shift;
  wire [4:0] dec_shift_amount;
  wire dec_use_rs;
  wire dec_use_imm32;
  wire [3:0] dec_rs;
  wire [3:0] dec_rotate_imm;
  wire [7:0] dec_imm8;
  wire dec_is_load;
  wire dec_is_unsigned_byte;
  wire dec_is_not_postindex;
  wire dec_is_added_offset;
  wire dec_is_write_back;
  wire [11:0] dec_offset_12;
  wire dec_branch_with_link;
  wire [23:0] dec_signed_immmed_24;
  wire dec_mem_read;
  wire dec_mem_write;

  // memory control
  wire mem_write;
  wire mem_ready;

  assign neg_flag = cpsr[31];
  assign zero_flag = cpsr[30];
  assign carry_flag = cpsr[29];
  assign overflow_flag = cpsr[28];

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


  // barrel shifter
  reg [31:0] shift_value;
  reg [4:0] shift_amt;
  wire [31:0] shifter_operand;
  reg shfiter_use_imm32;
  // reg [1:0] shift_type;
  // reg shifter_carry_out;

  barrel_shifter barrel_shifter_inst(
    .shift_in(shift_value),
    .shift_type(dec_shift),
    .shift_imm(shift_amt),
    .rs(dec_rs[7:0]),
    .is_imm_32(shfiter_use_imm32),
    .is_use_rs(dec_use_rs),
    .carry_in(carry_flag),
    .shifter_operand(shifter_operand),
    .shift_carry_out(carry_flag)
  );

  // interal registers to communicate with ALU
  reg execute_enable;
  reg [31:0] alu_a;    // operand 1
  reg [31:0] alu_b;    // operand 2(shifter operand)
  wire [31:0] alu_out;  // result
  reg [3:0] alu_opcode;

  alu alu_inst (
    .enable(execute_enable),
    .opcode(alu_opcode),
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

  always @(posedge clk or negedge n_reset) begin
    if (!n_reset) begin
      for (int i = 0; i < 13; i = i + 1) begin
        register[i] <= 32'd0;
      end
      sp <= 32'd0;
      lr <= 32'd0;
      pc <= 32'd0;
    end else begin
      case (state)
        IDLE: begin
          $display("IDLE");
        end

        FETCH: begin
          $display("[CPU] FETCH");
          if (little_endian_en)  // for convinient, little->big
            ir <= (data_in[7:0] << 24) | (data_in[15:8] << 16) | (data_in[23:16] << 8) | (data_in[31:24]);
          else
            ir <= data_in;
          pc <= pc + 4;
        end

        DECODE: begin
          $display("[CPU] DECODE");
          // TODO: error check
          
        end

        EXECUTE: begin
          $display("[CPU] EXECUTE");
          case (ir[27:25])
            DATA_PROCESSING_REG: begin
              $display("[CPU] DATA_PROCESSING_REG");
              shift_value <= register[dec_rm];
              shift_amt <= dec_shift_amount;
              alu_b <= shifter_operand;
              alu_opcode <= dec_opcode;
            end

            DATA_PROCESSING_IMM: begin
              $display("[CPU] DATA_PROCESSING_IMM");
              shift_value <= dec_imm8;
              shift_amt <= dec_rotate_imm;
              shfiter_use_imm32 <= dec_use_imm32;
              alu_b <= shifter_operand;
              alu_opcode <= dec_opcode;
            end

            LOAD_STORE_IMM: begin
            end

            LOAD_STORE_REG: begin
            end

            BRANCH: begin
            end
            default:  $display("[CPU] UNKNOWN TYPE: %x", ir[27:25]);
          endcase

          alu_a <= register[dec_rn];
        end

        WRITE_BACK: begin
          $display("[CPU] WRITE_BACK");
          register[dec_rd] <= alu_out;
        end

        DONE: begin
          $display("[CPU] DONE");
        end
      endcase
    end
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
        next_state = FETCH;
      end
      FETCH: begin
        next_state = DECODE;
        decode_enable = 1;
      end
      DECODE: begin
        if (decode_valid) begin
          next_state = EXECUTE;
          execute_enable = 1;
        end
      end
      EXECUTE: begin
        next_state = WRITE_BACK;
        // next_state = EXECUTE;
      end
      WRITE_BACK: begin
        next_state = DONE;
      end
      DONE: begin
        next_state = FETCH;
      end
      default:
        next_state = IDLE;
    endcase
  end



endmodule
