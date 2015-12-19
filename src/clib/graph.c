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
    nodelist_t *temp = g->nodes;
    while(temp->next) {
        temp = temp->next;
        free(temp->previous);
    }
    free(temp);
    free(g);
}

/* check if graph contains data */
int contains(graph_t *g, void *data, int (* comp)(void *a, void *b)) {
    nodelist_t *temp = g->nodes;
    while(temp)
        if(comp(temp->node->data, data))
            return 1;
    return 0;
}

/* add a node to g by iterating through the list, returning if the node is found, and if not, adding it to the end */
void add_node(graph_t *g, node_t *node) {
    nodelist_t *n = (nodelist_t *)malloc(sizeof(nodelist_t));
    n->node = node;
    nodelist_t *temp = g->nodes;
    /* make temp point to last nodelist_t in g->nodes */
    if(temp) {
        while(temp->next) {
            if(temp->node == node)
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
    nodelist_t *temp = g->nodes;
    while(temp && temp->node != n)
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
    nodelist_t *temp;
    for(temp = b->nodes; temp; temp = temp->next)
        add_node(a, temp->node);
    return a;
}

/* removes all nodes of *right that exist in *left from *left */
graph_t *subtract_graphs(graph_t *left, graph_t *right) {
    nodelist_t *temp;
    for(temp = right->nodes; temp; temp = temp->next) {
        printf("removing temp = %x\n", (int) temp);
        int i = remove_node(left, temp->node);
        if(i)
            printf("not removed!\n");
    }
    return left;
}

graph_t *graph_copy(const graph_t *src) {
    graph_t *g = init_graph();
    nodelist_t *temp;
    for(temp = src->nodes; temp; temp = temp->next)
        add_node(g, temp->node);
    return g;
}

int graph_equals(const graph_t *a, const graph_t *b) {
    if(a == b)
        return 1;
    const nodelist_t *temp_a = a->nodes;
    const nodelist_t *temp_b = b->nodes;
    while(temp_a && temp_b) {
        if(temp_a->node != temp_b->node)
            return 0;
        temp_a = temp_a->next;
        temp_b = temp_b->next;
    }
    if(temp_a || temp_b)
        return 0;
    return 1;
}

