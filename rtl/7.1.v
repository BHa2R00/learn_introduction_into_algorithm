/*
 * reference: ../src/7.1.c
 * fsm1: exchange
 * fsm2: partition
 * fsm3: quicksort
 * fsm: main
 */

module quicksort(
	output reg [1:0] cst1, nst1, 
	output reg [3:0] cst2, nst2, 
	output reg [3:0] cst3, nst3, 
	output reg [2:0] cst, nst, 
	output full, empty, idle, 
	input push, pop, clear, sort, 
	output reg [15:0] tx_data, 
	input [15:0] rx_data, 
	input enable, 
	input rstn, clk 
);

reg [15:0] A[0:255];
reg [7:0] a_top;
assign empty = a_top == 0;
assign full = a_top == 255;
reg [7:0] i, j;
wire [15:0] A_i = A[i];
wire [15:0] A_j = A[j];
reg [15:0] e, x;
reg [8+8-1:0] pr[0:15];
reg [3:0] pr_top;
wire empty_pr = pr_top == 0;
reg [7:0] p, r, q;

wire [8+8-1:0] top_pr = pr[pr_top];
wire [3:0] pr_top_left = pr_top - 1;
wire [3:0] pr_top_right = pr_top + 1;
wire [7:0] a_top_left = a_top - 1;
wire [7:0] a_top_right = a_top + 1;

`ifndef GRAY
	`define GRAY(X) (X^(X>>1))
`endif

reg req1, ack1;
reg req2, ack2;
reg req3, ack3;

localparam [1:0]
	st1_end		= `GRAY(3),
	st1_j		= `GRAY(2),
	st1_i		= `GRAY(1),
	st1_idle	= `GRAY(0);
reg req1_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) req1_d <= 1'b0;
	else if(enable) req1_d <= req1;
end
wire req1_x = req1_d ^ req1;
always@(negedge rstn or posedge clk) begin
	if(!rstn) cst1 <= st1_idle;
	else if(enable) cst1 <= nst1;
	else cst1 <= st1_idle;
end
always@(*) begin
	case(cst1)
		st1_idle: nst1 = req1_x ? st1_i : cst1;
		st1_i: nst1 = st1_j;
		st1_j: nst1 = st1_end;
		st1_end: nst1 = st1_idle;
		default: nst1 = st1_idle;
	endcase
end
always@(negedge rstn or posedge clk) begin
	if(!rstn) ack1 <= 1'b0;
	else if(enable) begin
		case(nst1)
			st1_end: ack1 <= ~ack1;
			default: ack1 <= ack1;
		endcase
	end
end

localparam [3:0]
	st2_end		= `GRAY(10),
	st2_4		= `GRAY(9),
	st2_1		= `GRAY(8),
	st2_j		= `GRAY(7),
	st2_i		= `GRAY(6),
	st2_3		= `GRAY(5),
	st2_2		= `GRAY(4),
	st2_if		= `GRAY(3),
	st2_for		= `GRAY(2),
	st2_x		= `GRAY(1),
	st2_idle	= `GRAY(0);
reg req2_d, ack1_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		req2_d <= 1'b0;
		ack1_d <= 1'b0;
	end
	else if(enable) begin
		req2_d <= req2;
		ack1_d <= ack1;
	end
end
wire req2_x = req2_d ^ req2;
wire ack1_x = ack1_d ^ ack1;
always@(negedge rstn or posedge clk) begin
	if(!rstn) cst2 <= st2_idle;
	else if(enable) cst2 <= nst2;
	else cst2 <= st2_idle;
end
wire for_check = j != r;
wire if_exch = A_j < x;
always@(*) begin
	case(cst2)
		st2_idle: nst2 = req2_x ? st2_x : cst2;
		st2_x: nst2 = st2_for;
		st2_for: nst2 = for_check ? st2_if : st2_1;
		st2_if: nst2 = if_exch ? st2_2 : st2_j;
		st2_2: nst2 = st2_3;
		st2_3: nst2 = ack1_x ? st2_i : cst2;
		st2_i: nst2 = st2_j;
		st2_j: nst2 = st2_for;
		st2_1: nst2 = st2_4;
		st2_4: nst2 = ack1_x ? st2_end : cst2;
		st2_end: nst2 = st2_idle;
		default: nst2 = st2_idle;
	endcase
end
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		req1 <= 1'b0;
		ack2 <= 1'b0;
	end
	else if(enable) begin
		case(nst2)
			st2_2: req1 <= ~req1;
			st2_1: req1 <= ~req1;
			st2_end: ack2 <= ~ack2;
			default: begin
				req1 <= req1;
				ack2 <= ack2;
			end
		endcase
	end
end

localparam [3:0]
	st3_end		= `GRAY(9),
	st3_pushp	= `GRAY(8),
	st3_ifr		= `GRAY(7),
	st3_pushr	= `GRAY(6),
	st3_ifp		= `GRAY(5),
	st3_1		= `GRAY(4),
	st3_pop		= `GRAY(3),
	st3_while	= `GRAY(2),
	st3_push	= `GRAY(1),
	st3_idle	= `GRAY(0);
reg req3_d, ack2_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		req3_d <= 1'b0;
		ack2_d <= 1'b0;
	end
	else if(enable) begin
		req3_d <= req3;
		ack2_d <= ack2;
	end
end
wire req3_x = req3_d ^ req3;
wire ack2_x = ack2_d ^ ack2;
always@(negedge rstn or posedge clk) begin
	if(!rstn) cst3 <= st3_idle;
	else if(enable) cst3 <= nst3;
	else cst3 <= st3_idle;
end
wire [7:0] q_left = q - 1;
wire [7:0] q_right = q + 1;
wire if_p = (q_left > p);
wire if_r = (q_right < r);
always@(*) begin
	case(cst3)
		st3_idle: nst3 = req3_x ? st3_push : cst3;
		st3_push: nst3 = st3_while;
		st3_while: nst3 = empty_pr ? st3_end : st3_pop;
		st3_pop: nst3 = st3_1;
		st3_1: nst3 = ack2_x ? st3_ifp : cst3;
		st3_ifp: nst3 = if_p ? st3_pushr : st3_ifr;
		st3_pushr: nst3 = st3_ifr;
		st3_ifr: nst3 = if_r ? st3_pushp : st3_while;
		st3_pushp: nst3 = st3_while;
		st3_end: nst3 = st3_idle;
		default: nst3 = st3_idle;
	endcase
end
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		req2 <= 1'b0;
		ack3 <= 1'b0;
	end
	else if(enable) begin
		case(nst3)
			st3_pop: req2 <= ~req2;
			st3_end: ack3 <= ~ack3;
			default: begin
				req2 <= req2;
				ack3 <= ack3;
			end
		endcase
	end
end

localparam [2:0]
	st_1		= `GRAY(5),
	st_sort		= `GRAY(4),
	st_pop		= `GRAY(3),
	st_push		= `GRAY(2),
	st_clear	= `GRAY(1),
	st_idle		= `GRAY(0);
reg clear_d, push_d, pop_d, sort_d, ack3_d;
always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		clear_d <= 1'b0;
		push_d <= 1'b0;
		pop_d <= 1'b0;
		sort_d <= 1'b0;
		ack3_d <= 1'b0;
	end
	else if(enable) begin
		clear_d <= clear;
		push_d <= push;
		pop_d <= pop;
		sort_d <= sort;
		ack3_d <= ack3;
	end
end
wire clear_x = clear_d ^ clear;
wire push_x = push_d ^ push;
wire pop_x = pop_d ^ pop;
wire sort_x = sort_d ^ sort;
wire ack3_x = ack3_d ^ ack3;
always@(negedge rstn or posedge clk) begin
	if(!rstn) cst <= st_idle;
	else if(enable) cst <= nst;
	else cst <= st_idle;
end
always@(*) begin
	case(cst)
		st_idle: nst = clear_x ? st_clear : push_x ? st_push : pop_x ? st_pop : sort_x ? st_sort : cst;
		st_clear: nst = st_idle;
		st_push: nst = st_idle;
		st_pop: nst = st_idle;
		st_sort: nst = st_1;
		st_1: nst = ack3_x ? st_idle : cst;
		default: nst = st_idle;
	endcase
end
always@(negedge rstn or posedge clk) begin
	if(!rstn) req3 <= 1'b0;
	else if(enable) begin
		case(nst)
			st_sort: req3 <= ~req3;
			default: req3 <= req3;
		endcase
	end
end
assign idle = cst == st_idle;

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		i <= 8'd0;
		j <= 8'd0;
		x <= 16'd0;
		q <= 8'd0;
	end
	else if(enable) begin
		case(nst2)
			st2_x: begin
				x <= A[r];
				i <= p;
				j <= p;
			end
			st2_j: j <= j + 1;
			st2_i: i <= i + 1;
			st2_end: q <= i;
			default: begin
				i <= i;
				j <= j;
				x <= x;
				q <= q;
			end
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		pr_top <= 4'd0;
		p <= 8'd0;
		r <= 8'd0;
	end
	else if(enable) begin
		case(nst3)
			st3_push: begin
				pr[pr_top_right] <= {8'd1, a_top};
				pr_top <= pr_top_right;
			end
			st3_pop: begin
				p <= top_pr[8+8-1:8];
				r <= top_pr[8-1:0];
				pr_top <= pr_top_left;
			end
			st3_pushr: begin
				pr[pr_top_right] <= {p, q_left};
				pr_top <= pr_top_right;
			end
			st3_pushp: begin
				pr[pr_top_right] <= {q_right, r};
				pr_top <= pr_top_right;
			end
			default: begin
				pr_top <= pr_top;
				p <= p;
				r <= r;
			end
		endcase
	end
end

always@(negedge rstn or posedge clk) begin
	if(!rstn) begin
		e <= 16'd0;
		a_top <= 8'd0;
		tx_data <= 16'd0;
	end
	else if(enable) begin
		case(nst1)
			st1_i: begin 
				e <= A_i;
				A[i] <= A_j; 
			end
			st1_j: A[j] <= e;
			default: e <= e;
		endcase
		case(nst)
			st_clear: a_top <= 8'd0;
			st_push: begin
				A[a_top_right] <= rx_data;
				a_top <= a_top_right;
			end
			st_pop: begin
				tx_data <= A[a_top];
				a_top <= a_top_left;
			end
			default: begin
				a_top <= a_top;
				tx_data <= tx_data;
			end
		endcase
	end
end

endmodule
