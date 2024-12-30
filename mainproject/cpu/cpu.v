module cpu (
  input clk,
  input n_reset
);
  localparam IDLE = 2'b00, FETCH = 2'b01, DECODE = 2'b10, EXECUTE = 2'b11;

  localparam DATA_PROCESSING_AND = 4'b0000,
             DATA_PROCESSING_EOR = 4'b0001,
             DATA_PROCESSING_SUB = 4'b0010,
             DATA_PROCESSING_RSB = 4'b0011,
             DATA_PROCESSING_ADD = 4'b0100,
             DATA_PROCESSING_ADC = 4'b0101,
             DATA_PROCESSING_SBC = 4'b0110,
             DATA_PROCESSING_RSC = 4'b0111,
             DATA_PROCESSING_TST = 4'b1000,
             DATA_PROCESSING_TEQ = 4'b1001,
             DATA_PROCESSING_CMP = 4'b1010,
             DATA_PROCESSING_CMN = 4'b1011,
             DATA_PROCESSING_ORR = 4'b1100,
             DATA_PROCESSING_MOV = 4'b1101,
             DATA_PROCESSING_BIC = 4'b1110,
             DATA_PROCESSING_MVN = 4'b1111;


  reg [1:0] state;
  reg [1:0] next_state;

  // registers
  reg [31:0] register [0:12];
  reg [31:0] sp;  // R13
  reg [31:0] lr;  // R14
  reg [31:0] pc;  // R15

  reg [31:0] cpsr;

  reg [31:0] ir;  // instruction register

  // interfaces
  wire [31:0] address;
  wire [31:0] data_in;
  wire [31:0] data_out;

  // internal wires
  reg bram_enable;
  wire [17:0] bram_addr;

  reg little_endian;

  initial begin
    $monitor("[%0t] PC [%x] INST [0x%8x]",$time, pc, ir);
  end

  always @(posedge clk or negedge n_reset) begin
    case (state)
      // skip condition flag
      DECODE: begin
        // data processing
        if (ir[27:26] == 2'b00) begin
          case (ir[24:21])
            DATA_PROCESSING_MOV, DATA_PROCESSING_MVN: begin
              if (ir[25] == 1'b1) begin // 32-bit immediate
                register[ir[15:12]] <= (ir[7:0] >> (ir[11:8] * 2)) | (ir[7:0] << (32 - (ir[11:8] * 2)));
              end else begin
              end
            end
          endcase
        // load or store
        end else if(ir[27:26] == 2'b01) begin

        end
      end
      EXECUTE: begin
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
        pc = 32'h0000_0000;
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

  assign bram_addr = pc >> 2;

  // modules
  bram bram_inst (
    .clk(clk),
    .enable(bram_enable),
    .address(bram_addr),
    .data_out(data_in)
    );


endmodule
