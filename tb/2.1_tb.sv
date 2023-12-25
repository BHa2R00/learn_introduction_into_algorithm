//`define ASYNC
`include "../rtl/2.1.v"
`timescale 1ns/100ps

module insertion_sort_tb;

reg rstn, enable;
reg clk;
initial clk = 1'b0;
always #10.416667 clk = ~clk;
reg dut_clk;
reg [15:0] din;
wire [15:0] dout;
reg push, pop, clear, sort;
wire full, empty, idle;
wire [3:0] cst, nst;

`ifdef ASYNC
wire locale_clock;
wire [3:0] nst_d;
initial dut_clk = 0;
always #10.416667 dut_clk = locale_clock;
`else 
always@(*) dut_clk = clk;
`endif

insertion_sort dut(
`ifdef ASYNC
	.locale_clock(locale_clock), 
	.nst_d(nst_d), 
`endif
	.full(full), .empty(empty), .idle(idle), 
	.cst(cst), .nst(nst),
	.push(push), .pop(pop), .clear(clear), .sort(sort), 
	.dout(dout), 
	.din(din), 
	.enable(enable), 
	.rstn(rstn), .clk(dut_clk) 
);

task push_and_pop;
	begin
		$write("\n");
		repeat(2) @(negedge clk); clear = ~clear;
		$write("push\n");
		repeat($urandom_range(100,254)) begin
			din = $urandom_range(0,32768);
			$write("%d ", din);
			repeat(10) @(negedge clk);
			push = ~push;
			repeat(2) @(negedge clk);
		end
		$write("\n");
		repeat(2) @(negedge clk); sort = ~sort;
		$write("pop\n");
		@(posedge idle);
		while(!empty) begin
			repeat(10) @(negedge clk);
			if(idle) begin
				$write("%d ", dout);
				pop = ~pop;
			end
			repeat(2) @(negedge clk);
		end
		$write("\n");
	end
endtask

initial begin
	{rstn, enable, push, pop, clear, sort} = 0;
	din = $urandom_range(0,32768);
	repeat(2) @(negedge clk); rstn = 1'b1;
	repeat(2) @(negedge clk); enable = 1'b1;
	repeat(3) push_and_pop();
	repeat(2) @(negedge clk); enable = 1'b0;
	repeat(2) @(negedge clk); rstn = 1'b0;
	$finish;
end

initial begin
	$dumpfile("../work/insertion_sort_tb.fst");
	$dumpvars(0,insertion_sort_tb);
end

endmodule
