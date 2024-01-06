#include <stdio.h>

unsigned short urandom(){
	FILE* fp = fopen("/dev/urandom","rb");
	unsigned short r;
	fread(&r, sizeof(unsigned short), 1, fp);
	return r;
}

unsigned short A[256];
unsigned char a_top;
unsigned short a;
void push_a(unsigned short d){ 
	a_top++; 
	A[a_top] = d; 
}
void pop_a(){ a = A[a_top]; a_top--; }

unsigned char i, j;
unsigned short e;
#define exchange 			/* st1_idle ~req1 ? -> st1_i	 */\
	e = A[i]; A[i] = A[j];	/* st1_i -> st1_j 				 */\
	A[j] = e; 				/* st1_j -> st1_idle			 */
#define compare_exchange 	/* st2_idle ~req2 ? -> st2_if	 */\
	if(A[i] > A[j]){ 		/* st2_if -> st2_1 | st2_idle	 */\
		exchange 			/* st2_1 -> st2_2 :~req1		 */\
							/* st2_2 ack1 ? -> st2_idle		 */\
	}
unsigned char p, r;
#define insersort 							/* st3_idle ~req3 ? -> st3_init_p		 */\
											/* st3_init_p -> st3_for_p				 */\
	for(p = 1; p != a_top + 1; p++) {		/* st3_for_p -> st3_init_r | st3_idle	 */\
											/* st3_init_r -> st3_for_r				 */\
		for(r = (p - 1); r != 0; r--) { 	/* st3_for_r -> st3_i | st3_p			 */\
			i = r; 							/* st3_i -> st3_1						 */\
			j = r + 1; \
			compare_exchange 				/* st3_1 -> st3_2 :~req2				 */\
											/* st3_2 ack2 ? -> st3_r				 */\
		} 									/* st3_r -> st3_p						 */\
	}										/* st3_p -> st3_for_p					 */


int main(){
	int k;
	unsigned short u;
	for(k = 0; k < 254; k++){
		u = urandom();
		push_a(u);
	}
	insersort
	for(k = 0; k < 254; k++){ 
		pop_a();
		printf("%d %d\n", k, a);
	}
	return 0;
}
