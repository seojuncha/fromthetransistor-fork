module barrel_shifter (
  input clk,
  input [31:0] shift_in,   // rm
  input [1:0] shift_type,
  input [4:0] shift_imm,   // 5-bit, [0:31]
  input carry_in,
  output reg [31:0] shifter_operand,
  output reg shift_carry_out
);
  localparam LSL = 2'b00,  // logigal shift left
             LSR = 2'b01,  // logical shift right
             ASR = 2'b10,  // arithmetic shift right
             ROR = 2'b11;  // rotate right

  always @(posedge clk) begin
    case (shift_type)
      LSL: begin
        if (shift_imm == 0)
          shifter_operand = shift_in;
        else begin
          shifter_operand = (shift_in << shift_imm);
          shift_carry_out = shift_in[32-shift_imm];  // MSB
        end
      end
      LSR: begin
        if (shift_imm == 0) begin
          shifter_operand = 32'd0;
          shift_carry_out = shift_in[31];
        end else begin
          shifter_operand = (shift_in >> shift_imm);
          shift_carry_out = shift_in[shift_imm-1];  // LSB
        end
      end
      ASR: begin
        if (shift_imm == 0) begin
          if (shift_in[31] == 0) begin
            shifter_operand = 32'd0;
            shift_carry_out = shift_in[31];
          end else begin
            shifter_operand = 32'hffff_ffff;
            shift_carry_out = shift_in[31];
          end
        end else begin
          shifter_operand = $signed(shift_in) >>> shift_imm;
          shift_carry_out = shift_in[shift_imm-1];
        end
      end
      ROR: begin
        if (shift_imm == 0) begin
          // rrx operation
          shifter_operand = (shift_in >> 1) | (carry_in << 31);
          shift_carry_out = shift_in[0];
        end else begin
          shifter_operand = (shift_in >> shift_imm) | (shift_in << (32-shift_imm));
          shift_carry_out = shift_in[shift_imm-1];
        end
      end
      default: begin
      end
    endcase
  end

endmodule
