#include <stdio.h>

int main() {

    int x = 10;
    int i;
    for ( i = 2; i < x; i++) {
        if (x % i == 0)
            printf("%d is not prime\n", x);
            break;
    }

    return 0;
}
