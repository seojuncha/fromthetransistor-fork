module decoder (
  input clk,
  input enable,
  // common
  input [31:0] instruction,
  output reg [3:0] rd,
  output reg [3:0] rn,
  output reg [3:0] rm,
  // data processing
  output reg [3:0] opcode,
  output reg [1:0] shift, // also from load and store
  output reg [4:0] shift_amount,  // also from load and store
  output reg use_rs,
  output reg [3:0] rs,
  output reg use_imm32,
  output reg [3:0] rotate_imm,
  output reg [7:0] imm8,
  // load and store
  output reg is_load,
  output reg is_unsigned_byte,
  output reg is_not_postindex,
  output reg is_added_offset,
  output reg is_write_back,
  output reg [11:0] offset_12,
  // branch
  output reg branch_with_link,
  output reg [23:0] signed_immmed_24,
  // memory access
  output reg mem_read,
  output reg mem_write,
  // success or not
  output reg valid
);
  localparam DATA_PROCESSING_REG = 3'b000,
             DATA_PROCESSING_IMM = 3'b001,
             LOAD_STORE_IMM = 3'b010,
             LOAD_STORE_REG = 3'b011,
             BRANCH = 3'b101;

  always @(posedge clk) begin
    if (enable) begin
      valid <= 1; // Focibly 1 now.
      case (instruction[27:25])
        DATA_PROCESSING_REG: begin
          $display("[DECODER] DATA_PROCESSING_REG");
          shift <= instruction[6:5];
          if (instruction[20]) begin
            use_rs <= instruction[20];
            rs <= instruction[11:8];
          end else begin
            shift_amount <= instruction[11:7];
          end
          use_imm32 <= 0;
        end

        DATA_PROCESSING_IMM: begin
          $display("[DECODER] DATA_PROCESSING_IMM");
          rotate_imm <= instruction[11:8];
          imm8 <= instruction[7:0];
          use_imm32 <= 1;
        end

        LOAD_STORE_IMM: begin
          $display("[DECODER] LOAD_STORE_IMM");
          offset_12 <= instruction[11:0];
        end

        LOAD_STORE_REG: begin
          $display("[DECODER] LOAD_STORE_REG");
          if (instruction[11:4] != 8'd0) begin
            shift_amount <= instruction[11:7];
            shift <= instruction[6:5];
          end
        end

        BRANCH: begin
          $display("[DECODER] BRANCH");
          branch_with_link <= instruction[24];
          signed_immmed_24 <= instruction[23:0];
          mem_read <= 1'b0;
          mem_write <= 1'b0;
        end
      endcase

      if (instruction[27:25] == DATA_PROCESSING_REG || instruction[27:25] == DATA_PROCESSING_IMM) begin
        $display("[DECODER] is data processing: %b", instruction[24:21]);
        opcode <= instruction[24:21];
        mem_read <= 1'b0;
        mem_write <= 1'b0;
      end

      if (instruction[27:25] != BRANCH) begin
        $display("[DECODER] not branch");
        rn <= instruction[19:16];
        rd <= instruction[15:12];
      end

      if (instruction[27:25] == DATA_PROCESSING_REG || instruction[27:25] == LOAD_STORE_REG) begin
        $display("[DECODER] use register");
        rm <= instruction[3:0];
      end

      if (instruction[27:25] == LOAD_STORE_REG || instruction[27:25] == LOAD_STORE_IMM) begin
        is_not_postindex <= instruction[24];  // P bit
        is_added_offset <= instruction[23];   // U bit
        is_unsigned_byte <= instruction[22];  // B bit
        is_write_back <= instruction[21];     // W bit
        is_load <= instruction[20];           // L bit
        if (instruction[20]) begin
          mem_read <= 1;
          mem_write <= 0;
        end else begin
          mem_read <= 0;
          mem_write <= 1;
        end
      end
    end else begin
      valid <= 0;
    end
  end

endmodule
