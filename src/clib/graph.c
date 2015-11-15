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

/* add a node to a g by pushing it to the front of the node list */
void add_node(graph_t *g, node_t *node) {
    nodelist_t *n = (nodelist_t *)malloc(sizeof(nodelist_t));
    n->node = node;
    n->next = g->nodes;
    n->previous = NULL;
    /* check if graph is currently empty */
    if(g->nodes)
        g->nodes->previous = n;
    g->nodes = n;
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

graph_t *add_graphs(graph_t *a, graph_t *b) {
    nodelist_t *temp;
    for(temp = b->nodes; temp; temp = temp->next)
        add_node(a, temp->node);
    return a;
}

/* removes all nodes of *right that exist in *left from *left */
graph_t *subtract_graphs(graph_t *left, graph_t *right) {
    nodelist_t *temp = right->nodes;
    for(temp = right->nodes; temp; temp = temp->next) {
        printf("removing temp = %x\n", (int) temp);
        int i = remove_node(left, temp->node);
        if(i)
            printf("not removed!\n");
    }
    return left;
}