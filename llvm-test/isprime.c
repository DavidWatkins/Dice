#include <stdio.h>
#include <stdlib.h>

int main() {
	int b = 10;	
	int dim2 = 20;
	int* a = malloc(b * dim2);

	a[1 * 10] = 10;

	exit(0);

    return 0;
}
