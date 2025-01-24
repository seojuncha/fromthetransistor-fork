/**
  * Flash doesn't have nRST port. 
  * Because the flash memory doesn't have to clear data even thouth a reset signal is low.
  */
module flash(
  input clk,
  input rd_en,
  input wr_en,
  input erase_en,
  input [11:0] addr,
  input [31:0] idata,
  output reg[31:0] odata,
  output reg busy,
  output reg error
);
  localparam IDLE = 2'b00, READ = 2'b01, WRITE = 2'b10, ERASE = 2'b11;
  reg [1:0] state;

  reg [31:0] memory [0:1023];

  // Only for debug
  initial begin
    $readmemb("bootrom.bin.txt", memory);
  end

  always @(posedge clk) begin
    case (state)
      IDLE: begin
        busy <= 0;
        error <= 0;

        if (rd_en) begin
          odata <= memory[addr];
          state <= READ;
        end else if (wr_en) begin
          // Already cleaned~
          if (memory[addr] == 32'd0) begin
            memory[addr] <= idata;
            state <= WRITE;
          end else begin
            $display("[MEM][FLASH] write to non-erased area");
            error <= 1;
          end
        end else if (erase_en) begin
          // CHECK: Erase whole memory? or from addr?
          integer i;
          for (i = 0; i < 1023; i=i+1) begin
            memory[i] <= 32'd0;
          end
          state <= ERASE;
        end

      end

      READ: begin
        state <= IDLE;
      end

      WRITE: begin
        busy <= 1;
        state <= IDLE;
      end

      ERASE: begin
        busy <= 1;
        state <= IDLE;
      end
    endcase
  end

endmodule
