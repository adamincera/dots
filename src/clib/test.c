#include <stdio.h>
#include "translations.h"

int main() {
    list_t *l = range(0, 10);
    print_range(l);
    free_range(l);
    l = range(10, 0);
    print_range(l);
    free_range(l);
    l = range(20, 30);
    print_range(l);
    free_range(l);
    l = range(20, 20);
    print_range(l);
    free_range(l);
    return 0;
}
