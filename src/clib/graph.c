#include <stdlib.h>
#include <stdio.h>
#include "graph.h"

/* initialize empty graph */
graph_t *init_graph() {
    graph_t *g = (graph_t *) malloc(sizeof(graph_t));
    g->nodes = NULL;
    g->count = 0;
    return g;
}

/* deallocate graph */
void free_graph(graph_t * g) {
    if(g == NULL)
        return;
    list_t *temp = g->nodes;
    while(temp->next) {
        temp = temp->next;
        free(temp->previous);
    }
    free(temp);
    free(g);
}

/* check if graph contains data */
int contains(graph_t *g, void *data, int (* comp)(void *a, void *b)) {
    list_t *temp = g->nodes;
    while(temp)
        if(comp(((node_t *) temp->data)->data, data))
            return 1;
    return 0;
}

/* add a node to g by iterating through the list, returning if the node is found, and if not, adding it to the end */
void add_node(graph_t *g, const node_t *node) {
    list_t *n = (list_t *)malloc(sizeof(list_t));
    n->data = (void *) node;
    list_t *temp = g->nodes;
    /* make temp point to last list_t in g->nodes */
    if(temp) {
        while(temp->next) {
            if(temp->data == node)
                return;
            temp = temp->next;
        }
        temp->next = n;
    } else {
        g->nodes = n;
    }
    n->previous = temp;
    n->next = NULL;
    g->count++;
}

    
/* returns 0 on success, 1 if node not found */
int remove_node(graph_t *g, node_t *n) {
    if(n == NULL)
        return 1;
    list_t *temp = g->nodes;
    while(temp && temp->data != n)
        temp = temp->next;
    if(temp == NULL)
        return 1;
    if(temp->previous)
        temp->previous->next = temp->next;
    else
        g->nodes = temp->next;
    if(temp->next)
        temp->next->previous = temp->previous;
    if(!temp->next && !temp->previous) {
        g->nodes = 0;
    }
    free(temp);
    g->count--;
    return 0;
}

/* returns a graph containing all nodes in *a and all nodes in *b */
graph_t *plus(const graph_t *a, const graph_t *b) {
    graph_t *g = (graph_t *) malloc(sizeof(graph_t));
    g->nodes = NULL;
    g->count = 0;
    plus_equals(g, a);
    plus_equals(g, b);
    return g;
}

/* adds all nodes from *b to *a. returns a */
graph_t *plus_equals(graph_t *a, const graph_t *b) {
    list_t *temp;
    for(temp = b->nodes; temp; temp = temp->next)
        add_node(a, temp->data);
    return a;
}

/* removes all nodes of *right that exist in *left from *left */
graph_t *minus(const graph_t *left, const graph_t *right) {
    list_t *temp;
    graph_t *copy = graph_copy(left);
    for(temp = right->nodes; temp; temp = temp->next) {
        printf("removing temp = %x\n", (int) temp);
        int i = remove_node(copy, temp->data);
        if(i)
            printf("not removed!\n");
    }
    return copy;
}

graph_t *graph_copy(const graph_t *src) {
    graph_t *g = init_graph();
    list_t *temp;
    for(temp = src->nodes; temp; temp = temp->next)
        add_node(g, temp->data);
    return g;
}

int graph_equals(const graph_t *a, const graph_t *b) {
    if(a == b)
        return 1;
    const list_t *temp_a = a->nodes;
    const list_t *temp_b = b->nodes;
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

graph_t *graph_plus_node(const graph_t *g, const node_t *n) {
    graph_t *copy = graph_copy(g);
    add_node(copy, n);
    return copy;
}

graph_t *node_plus_node(const node_t *n1, const node_t *n2) {
    graph_t *ret = init_graph();
    add_node(ret, n1);
    add_node(ret, n2);
    return ret;
}
