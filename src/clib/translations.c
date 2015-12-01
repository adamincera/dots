#include "translations.h"
#include <stdio.h>
#include <stdlib.h>

list_t *range(int a, int b) {
    list_t *r = 0;
    int i;
    list_t *t;
    int *j;
    int sign = a > b ? 1 : -1;
    for(i = b; i != a + sign; i += sign) {
        t = (list_t *) malloc(sizeof(list_t));
        j = (int *) malloc(sizeof(int));
        *j = i;
        t->data = j;
        if(r)
            r->previous = t;
        t->next = r;
        r = t;
    }
    return r;
}

void free_range(list_t *r) {
    if(r->next)
    for(r = r->next; r->next; r = r->next) {
        free(r->previous->data);
        free(r->previous);
    }
    free(r->data);
    free(r);
}

void print_range(list_t *r) {
    printf("[");
    for(; r; r = r->next)
        if(r->next)
            printf("%d, ", *((int *) r->data));
        else
            printf("%d]\n", *((int *) r->data));
}
