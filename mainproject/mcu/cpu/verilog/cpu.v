module cpu (
  input clk,
  input rst,
  input little_endian_en,
  output [31:0] memory_addr,
  input [31:0] data_from_memory,
  output reg[31:0] data_to_memory,
  output reg read_from_memory,
  output reg write_to_memory,
  input memory_error
);
  localparam IDLE = 3'b000, 
             FETCH = 3'b001, 
             DECODE = 3'b010, 
             EXECUTE = 3'b011,
             WRITE_BACK = 3'b100,
             DONE = 3'b101;

  // localparam DATA_PROCESSING_REG = 3'b000,
  //            DATA_PROCESSING_IMM = 3'b001,
  //            LOAD_STORE_IMM = 3'b010,
  //            LOAD_STORE_REG = 3'b011,
  //            BRANCH = 3'b101;

  reg [2:0] state;
  reg [2:0] next_state;

  reg [31:0] instruction_register;
  reg [31:0] address_register, read_data_register, write_data_register;

  reg [31:0] general_register[0:14];
  reg [31:0] pc;

  assign memory_addr = (state == FETCH) ? pc : address_register;

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

  instruction_decoder instruction_decoder(
    .clk(clk),
    .enable(decode_enable),
    .instruction(instruction_register),
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

  barrel_shifter barrel_shifter_inst(
    .shift_in(shift_value),
    .shift_type(dec_shift),
    .shift_imm(shift_amt),
    .rs(dec_rs[7:0]),
    .is_imm_32(dec_use_imm32),
    .is_use_rs(dec_use_rs),
    .carry_in(carry_flag),
    .shifter_operand(shifter_operand),
    .shift_carry_out(carry_flag)
  );

  // interal registers to communicate with ALU
  reg execute_enable;
  reg [31:0] alu_a;     // operand 1. operand 2 from shifter
  wire [31:0] alu_out;  // result

  alu alu_inst (
    .enable(execute_enable),
    .opcode(dec_opcode),
    .operand1(alu_a),
    .operand2(shifter_operand),
    .carry_in(carry_flag),
    .result(alu_out),
    .negative_flag(neg_flag),
    .zero_flag(zero_flag),
    .carry_out_flag(carry_flag)
  );

  always @(posedge clk) begin
    case (state)
      IDLE: begin
        integer i;
        for (i = 0; i < 15; i = i + 1) begin
          general_register[i] <= 32'd0;
        end

        pc <= 32'h0000_0000;
        address_register <= 32'h0000_0000;
        read_data_register <= 32'd0;
        write_data_register <= 32'd0;

        read_from_memory <= 1;
        write_to_memory <= 0;
      end

      FETCH: begin
        address_register <= pc + 4;
        pc <= pc + 4;

        // for convinient, little->big
        if (little_endian_en)
          instruction_register <= (data_from_memory[7:0] << 24)
                                  | (data_from_memory[15:8] << 16)
                                  | (data_from_memory[23:16] << 8)
                                  | (data_from_memory[31:24]);
        else
          instruction_register <= data_from_memory;
      end

      DECODE: begin

      end

      EXECUTE: begin
        if (dec_use_imm32) begin
          shift_value <= dec_imm8;
          shift_amt <= dec_rotate_imm;
        end else begin
          shift_value <= general_register[dec_rm];
          shift_amt <= dec_shift_amount;
        end
        // reg_read_addr <= dec_rn;
        // $display("[%0t][CPU][CORE][EXECUTE] instruction: 0x%08x", $realtime, instruction_register);
        // case (ir[27:25])
        //   DATA_PROCESSING_REG: begin
        //     $display("[CPU] DATA_PROCESSING_REG");
        //     shift_value <= register[dec_rm];
        //     shift_amt <= dec_shift_amount;
        //     $display("[CPU] -------------------------------");
        //   end

        //   DATA_PROCESSING_IMM: begin
        //     $display("[CPU] DATA_PROCESSING_IMM");
        //     shift_value <= dec_imm8;
        //     shift_amt <= dec_rotate_imm;
        //     $display("[CPU] -------------------------------");
        //   end

        //   LOAD_STORE_IMM: begin
        //     $display("[CPU] LOAD_STORE_IMM");
        //     if (dec_is_added_offset)
        //       address <= dec_rn + dec_offset_12;
        //     else
        //       address <= dec_rn - dec_offset_12;

        //     $display("[CPU] -------------------------------");
        //   end

        //   LOAD_STORE_REG: begin
        //     $display("[CPU] LOAD_STORE_REG");
        //     if (dec_is_added_offset)
        //       address <= dec_rn + shifter_operand;
        //     else
        //       address <= dec_rn - shifter_operand;
        //     $display("[CPU] -------------------------------");
        //   end

        //   BRANCH: begin
        //     $display("[CPU] BRANCH");
        //     $display("[CPU] -------------------------------");
        //   end
        //   default:  $display("[CPU] Unknown instruction type: %x", ir[27:25]);
        // endcase

        alu_a <= general_register[dec_rn];
      end

      WRITE_BACK: begin
        general_register[dec_rd] <= alu_out;
      end

      DONE: begin
      end
    endcase
  end

  always @(posedge clk or negedge rst) begin
    if (!rst) state <= IDLE;
    else state <= next_state;
  end

  always @(*) begin
    decode_enable = 0;
    execute_enable = 0;
    // read_from_memory = 0;
    // write_to_memory = 0;

    next_state = state;

    case (state)
      IDLE: begin
        if (!memory_error)
          next_state = FETCH;
      end

      FETCH: begin
        if (!memory_error) begin
          next_state = DECODE;
        end
      end

      DECODE: begin
        decode_enable = 1;
        next_state = EXECUTE;
      end

      EXECUTE: begin
        execute_enable = 1;
        next_state = WRITE_BACK;
      end

      WRITE_BACK: begin
        execute_enable = 1;
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
