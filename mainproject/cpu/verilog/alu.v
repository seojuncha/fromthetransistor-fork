module alu (
  input [3:0] opcode,
  input [31:0] operand1,
  input [31:0] operand2,
  input carry_in,
  input enable_flag_update,  // enable CPSR flag update, s suffix
  output reg [31:0] result,
  output reg negative_flag,
  output reg zero_flag,
  output reg carry_out_flag,
  output reg overflow_flag
);
  reg [31:0] alu_out;

  initial alu_out = 0;

  localparam OPCODE_AND = 4'b0000,
             OPCODE_EOR = 4'b0001,
             OPCODE_SUB = 4'b0010,
             OPCODE_RSB = 4'b0011,
             OPCODE_ADD = 4'b0100,
             OPCODE_ADC = 4'b0101,
             OPCODE_SBC = 4'b0110,
             OPCODE_RSC = 4'b0111,
             OPCODE_TST = 4'b1000,
             OPCODE_TEQ = 4'b1001,
             OPCODE_CMP = 4'b1010,
             OPCODE_CMN = 4'b1011,
             OPCODE_ORR = 4'b1100,
             OPCODE_MOV = 4'b1101,
             OPCODE_BIC = 4'b1110,
             OPCODE_MVN = 4'b1111;

  always @(*) begin
    case (opcode)
      // logical operations, do not update flags
      OPCODE_AND: result = operand1 & operand2;
      OPCODE_EOR: result = operand1 ^ operand2;
      OPCODE_ORR: result = operand1 | operand2;
      OPCODE_MOV: result = operand2;
      OPCODE_MVN: result = !operand2;
      OPCODE_BIC: begin
        result = operand1 & !(operand2);
        zero_flag = (result == 0);
        negative_flag = result[31];
      end

      OPCODE_SUB: begin
        result = operand1 - operand2;
        zero_flag = (result == 0);
      end
      OPCODE_RSB: begin
        result = operand2 - operand1;
        if (enable_flag_update) begin
          zero_flag = (result == 0);
          negative_flag = result[31];
        end
      end
      OPCODE_ADD: begin
         result = operand1 + operand2;
         if (enable_flag_update) begin
          zero_flag = (result == 0);
          negative_flag = result[31];
         end
      end
      OPCODE_ADC: begin
        result = operand1 + operand2 + carry_in;
        if (enable_flag_update) begin
          zero_flag = (result == 0);
          negative_flag = result[31];
        end
      end
      OPCODE_SBC: begin    // with carry
        result = operand1 - operand2 - !(carry_in);
        if (enable_flag_update) begin
          zero_flag = (result == 0);
          negative_flag = result[31];
        end
      end
      OPCODE_RSC: begin     // with carry
        result = operand2 - operand1 - !(carry_in);
        if (enable_flag_update) begin
          zero_flag = (result == 0);
          negative_flag = result[31];
        end
      end

      // update condition flags
      OPCODE_TST: begin
         alu_out = operand1 & operand2;
         if (enable_flag_update) begin
          zero_flag = (alu_out == 0);
          negative_flag = alu_out[31];
         end
      end
      OPCODE_TEQ: begin
         alu_out = operand1 ^ operand2;
         if (enable_flag_update) begin
          zero_flag = (alu_out == 0);
          negative_flag = alu_out[31];
         end
      end
      OPCODE_CMP: begin
         alu_out = operand1 - operand2;
         if (enable_flag_update) begin
          zero_flag = (alu_out == 0);
          negative_flag = alu_out[31];
         end
      end
      OPCODE_CMN: begin
        alu_out = operand1 + operand2;
        if (enable_flag_update) begin
          zero_flag = (alu_out == 0);
          negative_flag = alu_out[31];
        end
      end
    endcase
  end

endmodule
