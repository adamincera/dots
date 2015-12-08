#include "list.h"
#include <stdio.h>
#include <stdlib.h>

list_t *add_front(list_t *l, void *data) {
    list_t *new_node = (list_t *) malloc(sizeof(list_t));
    new_node->data = data;
    new_node->previous = NULL;
    new_node->next = l;
    if(l)
        l->previous = new_node;
    return new_node;
}

list_t *add_back(list_t *l, void *data) {
    list_t *new_node = (list_t *) malloc(sizeof(list_t));
    new_node->data = data;
    new_node->next = NULL;
    if(l) {
        list_t *temp = l;
        while(temp->next)
            temp = temp->next;
        temp->next = new_node;
        new_node->previous = temp;
        return l;
    } 
    new_node->previous = NULL;
    return new_node;
}

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

void print_strings(list_t *r) {
    printf("[");
    for(; r; r = r->next)
        if(r->next)
            printf("%s, ", ((char *) r->data));
        else
            printf("%s]\n", ((char *) r->data));
}

void free_list(list_t *r) {
    if(r->next)
        for(r = r->next; r->next; r = r->next) {
            free(r->previous->data);
            free(r->previous);
        }
    else
        free(r->data);
    free(r);
}
