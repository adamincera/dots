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

    char *s1 = "hello ";
    char *s2 = "world";
    l = NULL;
    l = add_front(l, s2);
    l = add_front(l, s1);
    print_strings(l);
    free_list(l);

    l = NULL;
    l = add_back(l, s1);
    l = add_back(l, s2);
    print_strings(l);
    free_list(l);
    return 0;
}
