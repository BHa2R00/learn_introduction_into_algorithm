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

module insertion_sort(
`ifdef ASYNC
	output locale_clock, 
 	output reg [3:0] nst_d, 
`endif
	output reg full, empty, idle, 
 	output reg [3:0] cst, nst, 
	input push, pop, clear, sort, 
	output reg [15:0] dout, 
	input [15:0] din, 
	input enable, 
	input rstn, clk 
);

reg push_d, pop_d, clear_d, sort_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		push_d <= 1'b0;
		pop_d <= 1'b0;
		clear_d <= 1'b0;
		sort_d <= 1'b0;
	end
	else if(enable) begin
		push_d <= push;
		pop_d <= pop;
		clear_d <= clear;
		sort_d <= sort;
	end
end
wire push_x = push ^ push_d;
wire pop_x = pop ^ pop_d;
wire clear_x = clear ^ clear_d;
wire sort_x = sort ^ sort_d;

`ifndef GRAY
	`define GRAY(X) (X^(X>>1))
`endif
localparam [3:0] 
	st_do_i_end	= `GRAY(11), st_do_i	= `GRAY(10), st_do_i_jmp	= `GRAY(9), st_do_i_init	= `GRAY(8), 
	st_do_j_end	= `GRAY(7),  st_do_j	= `GRAY(6),  st_do_j_jmp	= `GRAY(5), st_do_j_init	= `GRAY(4),
	st_pop	 	= `GRAY(3),  st_push 	= `GRAY(2),  st_clear 		= `GRAY(1),
	st_idle 	= `GRAY(0);

reg [15:0] A[0:255];
reg [7:0] p, j, i;
reg [15:0] key;
wire [7:0] pop_p = p - 8'd1;

`ifdef ASYNC
assign locale_clock = |(nst_d ^ cst);
always@(*) begin
	if(!rstn) nst_d = st_idle;
	else if(!clk) begin
		if(enable) nst_d = nst;
	end
end
always@(*) begin
	if(!rstn) cst = st_idle;
	else if(clk) begin
		if(enable) cst = nst_d;
	end
end
`else
always@(negedge rstn or posedge clk) begin
	if(!rstn) cst <= st_idle;
	else if(enable) cst <= nst;
end
`endif

always@(*) begin
	case(cst)
		st_idle: nst = clear_x ? st_clear : push_x ? st_push : pop_x ? st_pop : sort_x ? st_do_j_init : cst;
		st_clear: nst = st_idle;
		st_push: nst = st_idle;
		st_pop: nst = st_idle;
		st_do_j_init: nst = st_do_j_jmp;
		st_do_j_jmp: nst = (j == p) ? st_do_j_end : st_do_i_init;
		st_do_i_init: nst = st_do_i_jmp;
		st_do_i_jmp: nst = ((i == -8'd1) || (A[i] < key)) ? st_do_i_end : st_do_i;
		st_do_i: nst = st_do_i_jmp;
		st_do_i_end: nst = st_do_j;
		st_do_j: nst = st_do_j_jmp;
		st_do_j_end: nst = st_idle;
		default: nst = st_idle;
	endcase
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) p <= 8'd0;
	else if(enable) begin
		case(nst)
			st_clear: p <= 8'd0;
			st_push: p <= p + 8'd1;
			st_pop: p <= p - 8'd1;
			st_do_j_init: p <= p - 1'd1;
			st_do_j_end: p <= p - 1'd1;
			default: p <= p;
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) j <= 8'd0;
	else if(enable) begin
		case(nst)
			st_do_j_init: j <= 8'd1;
			st_do_j: j <= j + 8'd1;
			default: j <= j;
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) i <= 8'd0;
	else if(enable) begin
		case(nst)
			st_do_i_init: i <= j - 8'd1;
			st_do_i: i <= i - 8'd1;
			default: i <= i;
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		dout <= 16'd0;
		key <= 16'd0;
	end
	else if(enable) begin
		case(nst)
			st_push: A[p] <= din;
			st_pop: dout <= A[pop_p];
			st_do_j_jmp: key <= A[j];
			st_do_i: A[i + 8'd1] <= A[i];
			st_do_i_end: A[i + 8'd1] <= key;
			default: begin
				dout <= dout;
				key <= key;
			end
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		idle <= 1'b1;
		full <= 1'b0;
		empty <= 1'b1;
	end
	else if(enable) begin
		case(nst)
			st_idle: begin
				idle <= 1'b1;
				full <= p == 8'd255;
				empty <= p == 8'd0;
			end
			default: begin
				idle <= 1'b0;
				full <= full;
				empty <= empty;
			end
		endcase
	end
end

endmodule
