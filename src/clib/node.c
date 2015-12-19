#include <stdlib.h>
#include <stdio.h>
#include "node.h"

/* initialize a new node that contains *data */
node_t *init_node(char *data) {
    node_t *n = (node_t *) malloc(sizeof(node_t));
    n->data = data;
    n->in = NULL;
    n->out = NULL;
    return n;
}

void free_node(node_t *n) {
    printf("freeing node at %x\n", (int) n);
    free(n->data);
    free(n);
}

/* TODO decide on default behavior */
/* compares node data, should return 0 if a == b, 1 if a > b, and -1 if a < b */
int node_compare(node_t *a, node_t *b, int (* comp)(void *a, void *b)) {
    /*
    if(comp == NULL)
        return *(a->data) == *(b->data);
        */
    return comp(a->data, b->data);
}

/* create undirected edge of weight 0 */
void connect_undir(node_t *a, node_t *b) {
    connect_dir(a, b);
    connect_dir(b, a);
}

/* create weighted undirected edge */
void connect_undir_weighted(node_t *a, node_t *b, float weight) {
    connect_dir_weighted(a, b, weight);
    connect_dir_weighted(b, a, weight);
}

/* create directed edge of weight 0 */
void connect_dir(node_t *src, node_t *dst) {
    connect_dir_weighted(src, dst, 0);
}

/* create weighted directed edge */
void connect_dir_weighted(node_t *src, node_t *dst, float weight) {

    /* add dst to src->out */
    edgelist_t *e = (edgelist_t *) malloc(sizeof(edgelist_t));
    e->node = dst;
    e->weight = weight;
    e->previous = NULL;
    e->next = src->out;
    if(src->out != NULL)
        src->out->previous = e;
    src->out = e;

    /* add src to dst->in */
    edgelist_t *f = (edgelist_t *) malloc(sizeof(edgelist_t));
    f->node = src;
    f->weight = weight;
    f->previous = NULL;
    f->next = dst->in;
    if(dst->in != NULL)
        dst->in->previous = f;
    dst->in = f;
}

/* remove directed edge from src to dst */
void remove_dir_edge(node_t *src, node_t *dst) {
    edgelist_t *e = src->out;
    while(e && e->node != dst)
        e = e->next;
    if(!e) {
        printf("there is no edge from %s to %s\n", (char *) src->data, (char *) dst->data);
        return;
    }

    edgelist_t *f;
    f = e->node->in;
    while(f && f->node != src)
        f = f->next;
    if(!f)
        printf("f is NULL\n");
    else {
        if(f->previous)
            f->previous->next = f->next;
        if(f->next)
            f->next->previous = f->previous;
        if(!f->next && !f->previous) {
            e->node->in = 0;
        }
        free(f);
    }
    if(e->previous)
        e->previous->next = e->next;
    if(e->next)
        e->next->previous = e->previous;
    if(!(e->next || e->previous))
        src->out = 0;
    free(e);
    e = NULL;

    /*
    e = src->out;
    f = e->node->in;
    while(f && f->node != src)
        f = f->next;
    if(!f)
        printf("f is NULL\n");
    else {
        if(f->previous)
            f->previous->next = f->next;
        if(f->next)
            f->next->previous = f->previous;
        if(!f->next && !f->previous) {
            e->node->out = 0;
        }
        free(f);
    }
    free(e);
    e = NULL;
    */
}

/* remove undirected edge between a and b */
void remove_undir_edge(node_t *a, node_t *b) {
    remove_dir_edge(a, b);
    remove_dir_edge(b, a);
}

char * value(node_t *n) {   
    return (n->data);
}
