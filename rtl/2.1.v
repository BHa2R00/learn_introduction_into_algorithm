/*
 * inserortion_sort(A)
 * 	for j = 1 to A.length - 1 
 * 		key = A[j]
 * 		//insert A[j] into the sorted sequence A[1..j-1]
 * 		i = j - 1
 * 		while i < 0 nor A[i] < key
 * 			A[i + 1] = A[i]
 * 			i = i - 1
 * 		A[i + 1] = key
 */
`ifndef gray
`define gray(x) (x^(x>>1))
`endif
module insertion_sort(
	output reg full, empty, idle, 
	input push, pop, clear, sort, 
	output reg [15:0] dout, 
	input [15:0] din, 
	input enable, 
	input rstn, clk 
);

reg [3:0] cst;
localparam [3:0] 
	st_do_i_end	= `gray(11), st_do_i	= `gray(10), st_do_i_jmp	= `gray(9), st_do_i_init	= `gray(8), 
	st_do_j_end	= `gray(7),  st_do_j	= `gray(6),  st_do_j_jmp	= `gray(5), st_do_j_init	= `gray(4),
	st_pop	 	= `gray(3),  st_push 	= `gray(2),  st_clear 		= `gray(1),
	st_idle 	= 0;
always@(*) idle = cst == st_idle;

reg [15:0] A[0:255];
reg [7:0] p, j, i;
always@(*) full = p == 8'd255;
always@(*) empty = p == 8'd0;
reg [15:0] key;
wire [7:0] pop_p = p - 8'd1;

reg [1:0] push_d, pop_d, clear_d, sort_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		push_d <= 2'b00;
		pop_d <= 2'b00;
		clear_d <= 2'b00;
		sort_d <= 2'b00;
	end
	else if(enable) begin
		push_d[1] <= push_d[0]; push_d[0] <= push;
		pop_d[1] <= pop_d[0]; pop_d[0] <= pop;
		clear_d[1] <= clear_d[0]; clear_d[0] <= clear;
		sort_d[1] <= sort_d[0]; sort_d[0] <= sort;
	end
end
wire push_01 = push_d == 2'b01;
wire pop_01 = pop_d == 2'b01;
wire clear_01 = clear_d == 2'b01;
wire sort_01 = sort_d == 2'b01;

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		dout <= 16'd0;
		cst <= st_idle;
		p <= 8'd0; j <= 8'd0; i <= 8'd0;
		key <= 16'd0;
	end
	else if(enable) begin
		case(cst)
			st_idle: begin
				if(clear_01) cst <= st_clear;
				else if(push_01) cst <= st_push;
				else if(pop_01) cst <= st_pop;
				else if(sort_01) cst <= st_do_j_init;
			end
			st_clear: begin cst <= st_idle; p <= 8'd0; end
			st_push: begin cst <= st_idle; p <= p + 8'd1; A[p] <= din; end
			st_pop: begin cst <= st_idle; p <= p - 8'd1; dout <= A[pop_p]; end
			st_do_j_init: begin cst <= st_do_j_jmp; j <= 8'd1; p <= p - 1'd1; end
			st_do_j_jmp: begin
				 key <= A[j];
				if(j == p) cst <= st_do_j_end;
				else cst <= st_do_i_init;
			end
			st_do_i_init: begin cst <= st_do_i_jmp; i <= j - 8'd1; end
			st_do_i_jmp: begin
				if((i == -8'd1) || (A[i] < key)) cst <= st_do_i_end;
				else cst <= st_do_i;
			end
			st_do_i: begin cst <= st_do_i_jmp; i <= i - 8'd1; A[i + 8'd1] <= A[i]; end
			st_do_i_end: begin cst <= st_do_j; A[i + 8'd1] <= key; end
			st_do_j: begin cst <= st_do_j_jmp; j <= j + 8'd1; end
			st_do_j_end: begin cst <= st_idle; p <= p - 1'd1; end
			default: begin
				cst <= st_idle;
				dout <= dout;
				p <= p; j <= j; i <= i;
				key <= key;
			end
		endcase
	end
end

endmodule
