#include <stdio.h>

struct s {
int five;
};

int fib(int x) {
	if (x < 2)
		return 1;
	else
		return fib(x-1) + fib(x-2);
}

struct s test(struct s x) {
	printf("%d", x.five);
return x;
}

int main() {	
	struct s y;
	// y.five = 10;
	// y = test(y);
	y.five = fib(10);

    printf("Hello %d", y.five);

    return 0;
}
