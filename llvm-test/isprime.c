#include <stdio.h>

struct s {
int five;
};

struct s test(struct s x) {
	printf("%d", x.five);
return x;
}

int main() {	
	struct s y;
	y.five = 10;
	y = test(y);

    printf("Hello %d", y.five);

    return 0;
}
