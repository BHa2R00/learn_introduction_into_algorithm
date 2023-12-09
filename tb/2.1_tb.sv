`timescale 1ns/100ps

`include "../rtl/2.1.v"

module insertion_sort_tb;

reg rstn, enable;
reg clk;
initial clk = 1'b0;
always #4.5 clk = ~clk;
reg [15:0] din;
wire [15:0] dout;
reg push, pop, clear, sort;
wire full, empty, idle;

insertion_sort dut(
	.full(full), .empty(empty), .idle(idle), 
	.push(push), .pop(pop), .clear(clear), .sort(sort), 
	.dout(dout), 
	.din(din), 
	.enable(enable), 
	.rstn(rstn), .clk(clk) 
);

initial begin
	{rstn, enable, push, pop, clear, sort} = 0;
	repeat(2) @(negedge clk); rstn = 1'b1;
	repeat(2) @(negedge clk); enable = 1'b1;
	repeat(2) @(negedge clk); clear = 1'b1;
	repeat(2) @(negedge clk); clear = 1'b0;
	$write("push\n");
	repeat($urandom_range(100,254)) begin
		din = $urandom_range(0,32768);
		$write("%d ", din);
		push = 1'b0;
		repeat(10) @(negedge clk);
		push = 1'b1;
		repeat(2) @(negedge clk);
	end
	$write("\n");
	sort = 1'b0;
	repeat(2) @(negedge clk); sort = 1'b1;
	$write("pop\n");
	while(!empty) begin
		pop = 1'b0;
		repeat(10) @(negedge clk);
		pop = 1'b1;
		repeat(2) @(negedge clk);
		$write("%d ", dout);
	end
	$write("\n");
	repeat(2) @(negedge clk); enable = 1'b0;
	repeat(2) @(negedge clk); rstn = 1'b0;
	$finish;
end

initial begin
	$dumpfile("../work/insertion_sort_tb.fst");
	$dumpvars(0,insertion_sort_tb);
end

endmodule
