module cpu (
  input clk,
  input n_reset
);
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
  wire bram_enable;
  wire [17:0] bram_addr;

  initial begin
    $monitor("[%0t] PC [%x] INST [0x%8x]",$time, pc, ir);
  end

  always @(posedge clk or negedge n_reset) begin
    if (!n_reset) begin
      pc <= 32'h0000_0000;
    end else begin
      if (bram_enable) begin
        ir <= data_in;
        pc <= pc + 4;
      end
    end
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