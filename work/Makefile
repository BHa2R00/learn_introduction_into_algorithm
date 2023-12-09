insertion_sort_tb: ../tb/2.1_tb.sv
	rm -rfv ../work/insertion_sort_tb.fst
	iverilog -o ./a.out ../tb/2.1_tb.sv
	./a.out -fst
	gtkwave ../work/insertion_sort_tb.fst
