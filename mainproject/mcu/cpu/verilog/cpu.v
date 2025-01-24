module cpu (
  input clk,
  input rst,
  input little_endian_en,
  output [31:0] memory_addr,
  input [31:0] data_from_memory,
  output reg[31:0] data_to_memory,
  output reg read_from_memory,
  output reg write_to_memory
);
  localparam REG_PC = 4'd15;

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

  reg [31:0] address_register, read_data_register, write_data_register;

  assign memory_addr = address_register;

  // reg [31:0] cpsr;
  // wire neg_flag;
  // wire zero_flag;
  // wire carry_flag;
  // wire overflow_flag;

  // assign neg_flag = cpsr[31];
  // assign zero_flag = cpsr[30];
  // assign carry_flag = cpsr[29];
  // assign overflow_flag = cpsr[28];

  // instruction register
  reg [31:0] instruction_register;

  // decoder interface
  // reg decode_enable;
  // wire decode_valid;
  // wire [3:0] dec_opcode;
  // wire [3:0] dec_rd;
  // wire [3:0] dec_rn;
  // wire [3:0] dec_rm;
  // wire [1:0] dec_shift;
  // wire [4:0] dec_shift_amount;
  // wire dec_use_rs;
  // wire dec_use_imm32;
  // wire [3:0] dec_rs;
  // wire [3:0] dec_rotate_imm;
  // wire [7:0] dec_imm8;
  // wire dec_is_load;
  // wire dec_is_unsigned_byte;
  // wire dec_is_not_postindex;
  // wire dec_is_added_offset;
  // wire dec_is_write_back;
  // wire [11:0] dec_offset_12;
  // wire dec_branch_with_link;
  // wire [23:0] dec_signed_immmed_24;
  // wire dec_mem_read;
  // wire dec_mem_write;

  // assign dec_mem_read = memory_read;
  // assign dec_mem_write = memory_write;

  reg [3:0] read_reg_addr;
  reg [31:0] read_data_from_reg;
  reg [3:0] write_reg_addr;
  reg [31:0] write_data_to_reg;

  register_files register_fies(
    .read_addr(read_reg_addr),
    .write_addr(write_reg_addr),
    .read_data(read_data_from_reg),
    .write_data(write_data_to_reg)
  );

  // decoder decoder_inst(
  //   .clk(clk),
  //   .enable(decode_enable),
  //   .instruction(ir),
  //   .opcode(dec_opcode),
  //   .rd(dec_rd),
  //   .rn(dec_rn),
  //   .rm(dec_rm),
  //   .shift(dec_shift),
  //   .shift_amount(dec_shift_amount),
  //   .use_rs(dec_use_rs),
  //   .use_imm32(dec_use_imm32),
  //   .rs(dec_rs),
  //   .rotate_imm(dec_rotate_imm),
  //   .imm8(dec_imm8),
  //   .is_load(dec_is_load),
  //   .is_unsigned_byte(dec_is_unsigned_byte),
  //   .is_not_postindex(dec_is_not_postindex),
  //   .is_added_offset(dec_is_added_offset),
  //   .is_write_back(dec_is_write_back),
  //   .offset_12(dec_offset_12),
  //   .branch_with_link(dec_branch_with_link),
  //   .signed_immmed_24(dec_signed_immmed_24),
  //   .mem_read(dec_mem_read),
  //   .mem_write(dec_mem_write),
  //   .valid(decode_valid)
  // );


  // // barrel shifter
  // reg [31:0] shift_value;
  // reg [4:0] shift_amt;
  // wire [31:0] shifter_operand;

  // barrel_shifter barrel_shifter_inst(
  //   .shift_in(shift_value),
  //   .shift_type(dec_shift),
  //   .shift_imm(shift_amt),
  //   .rs(dec_rs[7:0]),
  //   .is_imm_32(dec_use_imm32),
  //   .is_use_rs(dec_use_rs),
  //   .carry_in(carry_flag),
  //   .shifter_operand(shifter_operand),
  //   .shift_carry_out(carry_flag)
  // );

  // // interal registers to communicate with ALU
  // reg execute_enable;
  // reg [31:0] alu_a;     // operand 1. operand 2 from shifter
  // wire [31:0] alu_out;  // result

  // alu alu_inst (
  //   .enable(execute_enable),
  //   .opcode(dec_opcode),
  //   .operand1(alu_a),
  //   .operand2(shifter_operand),
  //   .carry_in(carry_flag),
  //   .result(alu_out),
  //   .negative_flag(neg_flag),
  //   .zero_flag(zero_flag),
  //   .carry_out_flag(carry_flag)
  // );

  // // only for debug
  // initial begin
  //   $monitor("[%0t] PC [%x] IR [0x%8x]",$time, pc, ir);
  //   for (int i = 0; i < 13; i = i + 1) begin
  //     register[i] <= 32'd0;
  //   end
  //   sp <= 32'd0;
  //   lr <= 32'd0;
  //   pc <= 32'd0;
  // end

  initial begin
    $monitor("[CPU][CORE] [%0t] [r=%b/w=%b]\n\t0x[%08x] read_from: 0x[%08x]\t\twrite_to: 0x[%08x]",
      $realtime, read_from_memory, write_to_memory, memory_addr, read_from_memory, write_to_memory);
  end

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      write_reg_addr <= REG_PC;
      write_data_to_reg <= 32'h0000_0000;

      address_register <= 32'd0;
      read_data_register <= 32'd0;
      write_data_register <= 32'd0;

      read_from_memory <= 1;
      write_to_memory <= 0;
    end else begin
      case (state)
        IDLE: begin
          $display("[CPU][IDLE][%0t]", $realtime);
          read_reg_addr <= REG_PC;
          write_reg_addr <= REG_PC;
        end

        FETCH: begin
          $display("[CPU][FETCH][%0t]", $realtime);
          address_register <= read_data_from_reg;
          read_from_memory <= 1;
          read_data_register <= data_from_memory;

          write_data_to_reg <= read_data_from_reg + 4;
        end

        DECODE: begin
          $display("[CPU][DECODE][%0t]", $realtime);
          // for convinient, little->big
          if (little_endian_en)
            instruction_register <= (read_data_register[7:0] << 24)
                                    | (read_data_register[15:8] << 16)
                                    | (read_data_register[23:16] << 8)
                                    | (read_data_register[31:24]);
          else
            instruction_register <= read_data_register;
        end

        EXECUTE: begin
          $display("[CPU][EXECUTE][%0t]", $realtime);
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

          // alu_a <= register[dec_rn];
        end

        WRITE_BACK: begin
          $display("[CPU][WRITE_BACK]");
          // $display("[CPU] alu %08x , %08x, %08x", alu_a, shifter_operand, alu_out);
          // register[dec_rd] <= alu_out;
        end

        DONE: begin
          $display("[CPU][DONE]");
        end
      endcase
    end
  end

  always @(posedge clk or negedge rst) begin
    if (!rst) begin
      state <= IDLE;
    end else begin
      state <= next_state;
    end
  end

  always @(*) begin
    // decode_enable = 0;
    // execute_enable = 0;
    // read_from_memory = 0;
    // write_to_memory = 0;

    next_state = state;

    case (state)
      IDLE: begin
        next_state = FETCH;
      end
      FETCH: begin
        $display("[CPU][CORE] state changed: IDLE -> FETCH");
        // decode_enable = 1;
        // read_from_memory = 1;
        next_state = DECODE;
      end
      DECODE: begin
        $display("[CPU][CORE] state changed: FETCH -> DECODE");
        next_state = EXECUTE;
        // if (decode_valid) begin
        //   next_state = EXECUTE;
        // end
      end
      EXECUTE: begin
        $display("[CPU][CORE] state changed: DECODE -> EXECUTE");
        next_state = WRITE_BACK;
        // next_state = EXECUTE;
      end
      WRITE_BACK: begin
        $display("[CPU][CORE] state changed: EXECUTE -> WRITE_BACK");
        next_state = DONE;
      end
      DONE: begin
        $display("[CPU][CORE] state changed: WRITE_BACK -> DONE");
        next_state = FETCH;
      end
      default:
        next_state = IDLE;
    endcase
  end



endmodule
