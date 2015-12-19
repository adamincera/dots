#include "dict.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static list_t *add_front(list_t *l, void *data, void *(*copy)(void *src)) {
    list_t *new_node = (list_t *) malloc(sizeof(list_t));
    if(copy)
        new_node->data = copy(data);
    else
        new_node->data = data;
    new_node->previous = NULL;
    new_node->next = l;
    if(l)
        l->previous = new_node;
    return new_node;
}

static void *void_strcpy(void *src) {
    int len = strlen(src);
    char *target = malloc(sizeof(char) * len + 1);
    strncpy((char *) target, (char *) src, len + 1);
    target[len] = 0;
    return (void *) target;
}

list_t *string_add_front(list_t *l, char *data) {
    return add_front(l, (void *) data, void_strcpy);
}

static void *float_copy(void *src) {
    float *dst = malloc(sizeof(float));
    *dst = *(float *) src;
    return dst;
}

list_t *num_add_front(list_t *l, float *data) {
    return add_front(l, (void *) data, float_copy);
}

static void *void_graph_copy(void *src) {
    return (void *) graph_copy(src);
}

list_t *graph_add_front(list_t *l, graph_t *data) {
    return add_front(l, (void *) data, void_graph_copy);
}

list_t *node_add_front(list_t *l, node_t *data) {
    return add_front(l, (void *) data, NULL);
}

/*
static void *void_dict_copy(entry_t **src) {
    return dict_copy(src);
}

list_t *dict_add_front(list_t *l, entry_t **data) {
    return add_front(l, (void *) data, void_dict_copy);
}

list_t *other_add_front(list_t *l, void *data) {
    return add_front(l, data, NULL);
}
*/

static list_t *add_back(list_t *l, void *data, void *(*copy)(void *src)) {
    list_t *new_node = (list_t *) malloc(sizeof(list_t));
    if(copy)
        new_node->data = copy(data);
    else
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

list_t *string_add_back(list_t *l, char *data) {
    return add_back(l, data, void_strcpy);
}

list_t *num_add_back(list_t *l, float *data) {
    return add_back(l, data, float_copy);
}

list_t *graph_add_back(list_t *l, graph_t *data) {
    return add_back(l, data, void_graph_copy);
}

list_t *node_add_back(list_t *l, node_t *data) {
    return add_back(l, data, NULL);
}

list_t *pop(list_t *l) {
    list_t *head = l->next;
    head->previous = NULL;
    free(l);
    return head;
}

void *peek(list_t *l) {
    return l->data;
}

list_t *range(int a, int b) {
    list_t *r = NULL;
    int i;
    list_t *t;
    float *j;
    int sign = a > b ? 1 : -1;
    for(i = b; i != a + sign; i += sign) {
        t = (list_t *) malloc(sizeof(list_t));
        j = (float *) malloc(sizeof(float));
        *j = (float) i;
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
            printf("%g, ", *((float *) r->data));
        else
            printf("%g", *((float *) r->data));
    printf("]\n");
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

list_t *num_list_copy(const list_t *src) {
    if(!src)
        return NULL;
    list_t *ret = NULL;
    const list_t *temp;
    for(temp = src; temp; temp = temp->next) {
        printf("here %x\n", temp);
        printf("copying %d\n", *(int *) temp->data);
        ret = num_add_back(ret, (float *) temp->data);
    }
    return ret;
}

list_t *string_list_copy(const list_t *src) { 
    if(!src)
        return NULL;
    list_t *ret = NULL;
    const list_t *temp;
    for(temp = src; temp; temp = temp->next) {
        printf("copying %s\n", (char *) temp->data);
        ret = string_add_back(ret, (char *) temp->data);
    }
    return ret;
}

list_t *node_list_copy(const list_t *src) { 
    if(!src)
        return NULL;
    list_t *ret = NULL;
    const list_t *temp;
    for(temp = src; temp; temp = temp->next) {
        printf("here %x\n", temp);
        printf("copying %d\n", *(int *) temp->data);
        ret = node_add_back(ret, (node_t *) temp->data);
    }
    return ret;
}

list_t *graph_list_copy(const list_t *src) { 
    if(!src)
        return NULL;
    list_t *ret = NULL;
    const list_t *temp;
    for(temp = src; temp; temp = temp->next) {
        printf("here %x\n", temp);
        printf("copying %d\n", *(int *) temp->data);
        ret = graph_add_back(ret, (graph_t *) temp->data);
    }
    return ret;
}

list_t *num_list_concat(const list_t *target, const list_t *src) {
    list_t *new_list = num_list_copy(target);
    if(new_list) {
        list_t *temp;
        for(temp = new_list; temp->next; temp = temp->next);
        temp->next = num_list_copy(src);
        return new_list;
    }
    return num_list_copy(src);
}

list_t *string_list_concat(const list_t *target, const list_t *src) {
    list_t *new_list = num_list_copy(target);
    if(new_list) {
        list_t *temp;
        for(temp = new_list; temp->next; temp = temp->next);
        temp->next = num_list_copy(src);
        return new_list;
    }
    return num_list_copy(src);
}

list_t *node_list_concat(const list_t *target, const list_t *src) {
    list_t *new_list = num_list_copy(target);
    if(new_list) {
        list_t *temp;
        for(temp = new_list; temp->next; temp = temp->next);
        temp->next = num_list_copy(src);
        return new_list;
    }
    return num_list_copy(src);
}

list_t *graph_list_concat(const list_t *target, const list_t *src) {
    list_t *new_list = num_list_copy(target);
    if(new_list) {
        list_t *temp;
        for(temp = new_list; temp->next; temp = temp->next);
        temp->next = num_list_copy(src);
        return new_list;
    }
    return num_list_copy(src);
}

void *list_access(const list_t *l, int i) {
    const list_t *temp;
    int j = 0;
    for(temp = l; temp; temp = temp->next) {
        if(j == i)
            return temp->data;
        j++;
    }
    return NULL;
}

int string_list_equals(const list_t *a, const list_t *b) {
    const list_t *temp_a = a;
    const list_t *temp_b = b;
    while(temp_a && temp_b) {
        if(strcmp((char *) temp_a->data, (char *) temp_b->data))
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

int num_list_equals(const list_t *a, const list_t *b) {
    const list_t *temp_a = a;
    const list_t *temp_b = b;
    while(temp_a && temp_b) {
        if(!float_equals(*(float *) temp_a->data, *(float *) temp_b->data))
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

int node_list_equals(const list_t *a, const list_t *b) {
    const list_t *temp_a = a;
    const list_t *temp_b = b;
    while(temp_a && temp_b) {
        if(temp_a->data != temp_b->data)
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

int graph_list_equals(const list_t *a, const list_t *b) {
    const list_t *temp_a = a;
    const list_t *temp_b = b;
    while(temp_a && temp_b) {
        if(!graph_equals((graph_t *) temp_a->data, (graph_t *) temp_b->data))
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

int other_list_equals(const list_t *a, const list_t *b, int (*comp)(void *a, void *b)) {
    const list_t *temp_a = a;
    const list_t *temp_b = b;
    while(temp_a && temp_b) {
        if(comp(temp_a->data, temp_b->data))
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

static void index_insert(list_t *l, int i, void *data, void *(*copy)(void *src)) {
    int j = 0;
    while(j < i) {
        j++;
        if(l->next)
            l = l->next;
        else
            break;
    }
    if(l->next) {
        if(copy)
            l->data = copy(data);
        else
            l->data = data;
    } else if(j == i) {
        l->next = (list_t *) malloc(sizeof(list_t));
        l->next->next = NULL;
        l->next->previous = l;
        if(copy)
            l->next->data = copy(data);
        else
            l->next->data = data;
    }
}

void num_index_insert(list_t *l, int i, float *a) {
    index_insert(l, i, a, float_copy);
}

void string_index_insert(list_t *l, int i, char *a) {
    index_insert(l, i, a, void_strcpy);
}

void node_index_insert(list_t *l, int i, node_t *a) {
    index_insert(l, i, a, NULL);
}

void graph_index_insert(list_t *l, int i, graph_t *a) {
    index_insert(l, i, a, void_graph_copy);
}

float num_list_min(list_t *l) {
    float min = (float) *(int *) l->data;
    for(; l; l = l->next)
        if(*(float *) l->data < min)
            min = (float) *(int *) l->data;
    return min;
}

float num_list_max(list_t *l) {
    float max = *(float *) l->data;
    for(; l; l = l->next)
        if(*(float *) l->data > max)
            max = *(float *) l->data;
    return max;
}

char  *string_list_min(list_t *l) {
    char *min = (char *) l->data;
    for(; l; l = l->next)
        if(strcmp((char *)l->data, min) < 0)
            min = (char *) l->data;
    return min;
}
    
char  *string_list_max(list_t *l) {
    char *max = (char *) l->data;
    for(; l; l = l->next)
        if(strcmp((char *)l->data, max) > 0)
            max = (char *) l->data;
    return max;
}

int list_len(list_t *l) {
    int len = 0;
    for(; l; l = l->next)
        len++;
    return len;
}
