module decoder (
  input clk,
  input enable,
  input [31:0] instruction,
  output reg [3:0] opcode,
  output reg [3:0] rd,
  output reg [3:0] rn,
  output reg [3:0] rm,
  output reg [1:0] shift,
  output reg [4:0] shift_amount,
  output reg use_rs,
  output reg [3:0] rs,
  output reg use_imm32,
  output reg [3:0] rotate_imm,
  output reg [7:0] imm8,  
  output reg mem_read,
  output reg mem_write,
);
  localparam DATA_PROCESSING_REG = 3'b000,
             DATA_PROCESSING_IMM = 3'b001,
             LOAD_STORE_IMM = 3'b010,
             LOAD_STORE_REG = 3'b011,
             BRANCH = 3'b101;

  always @(posedge clk) begin
    if (enable) begin
      case (instruction[27:25])
        DATA_PROCESSING_REG: begin
          opcode <= instruction[24:21];
          rd <= instruction[15:12];
          rn <= instruction[19:16];
          rm <= instruction[3:0]
          shift <= instruction[6:5];
          if (instruction[20]) begin
            use_rs <= instruction[20];
            rs <= instruction[11:8];
          end else begin
            shift_amount <= instruction[11:7];
          end
          mem_read <= 1'b0;
          mem_write <= 1'b0;
        end

        DATA_PROCESSING_IMM: begin
          opcode <= instruction[24:21];
          rd <= instruction[15:12];
          rn <= instruction[19:16];
          rotate_imm <= instruction[11:8];
          imm8 <= instruction[7:0];
          mem_read <= 1'b0;
          mem_write <= 1'b0;
        end

        LOAD_STORE_IMM: begin
        end

        LOAD_STORE_REG: begin
        end

        BRANCH: begin
        end
      endcase
    end
  end

endmodule
