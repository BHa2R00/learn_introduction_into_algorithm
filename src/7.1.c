#include <stdio.h>

unsigned short urandom(){
	FILE* fp = fopen("/dev/urandom","rb");
	unsigned short r;
	fread(&r, sizeof(unsigned short), 1, fp);
	return r;
}

/* quicksort
partition(A, p, r)
	x = A[r]
	i = p - 1
	for j = p to r - 1
		if A[j] <= x
			i = i + 1
			exchange A[i] with A[j]
	exchange A[i + 1] with A[r]
	return i + 1

quicksort(A, p, r)
	if p < r
		q = partition(A, p, r)
		quicksort(A, p, q - 1)
		quicksort(A, q + 1, r)

initial: quicksort(A, 1, A.length)
*/

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
	A[j] = e; 				/* st1_j -> st1_end	:~ack1		 */

unsigned short x;

unsigned short pr[8];
unsigned char pr_top;
unsigned char p, r;
void push_pr(unsigned char p, unsigned char r){ 
	pr_top++; 
	pr[pr_top] = ((unsigned short)(((p & 0xff)<<8) | (r & 0xff))); 
}
void pop_pr(){ 
	p = ((unsigned char)(0xff & (pr[pr_top] >> 8))); 
	r = ((unsigned char)(0xff & pr[pr_top])); 
	pr_top--; 
}
#define emptyp_pr (pr_top == 0)

unsigned char q;
void partition(){				/* st2_idle ~req2 ? -> st2_x	 */
	x = A[r];					/* st2_x -> st2_for	 			 */
	i = p;
	for(j = p; j != r; j++){	/* st2_for -> st2_if | st2_1	 */
		if(A[j] < x){			/* st2_if -> st2_2 | st2_for	 */
			exchange			/* st2_2 -> st2_3 :~req1		 */
								/* st2_3 ~ack1 ? -> st2_i		 */
			i++;				/* st2_i -> st2_for				 */
		}
	}
	exchange					/* st2_1 -> st2_4 :~req1		 */
								/* st2_4 ~ack1 ? -> st2_end		 */
	q = i;						/* st2_end :~ack2				 */
}

void quicksort(){				/* st3_idle ~req3 -> st3_push		 */
	push_pr(0, a_top);			/* st3_push -> st3_while			 */
	while(!emptyp_pr){			/* st3_while -> st3_pop | st3_end	 */
		pop_pr();				/* st3_pop -> st3_1 :~req2 			 */
		partition();			/* st3_1 ~ack2 ? -> st3_ifp			 */
		if(q - 1 > p) 			/* st3_ifp -> st3_pushr | st3_ifr	 */
			push_pr(p, q - 1);	/* st3_pushr -> st3_ifr				 */
		if(q + 1 < r) 			/* st3_ifr -> st3_pushp | st3_while	 */
			push_pr(q + 1, r);	/* st3_pushp -> st3_while			 */
	}
}								/* st3_end :~ack3					 */

int main(){
	int k;
	for(k = 0x0; k < 0x80; k++) push_a(urandom());
	quicksort();
	for(k = 0x0; k < 0x80; k++){ 
		pop_a();
		printf("%d %d\n", k, a);
	}
	return 0;
}
