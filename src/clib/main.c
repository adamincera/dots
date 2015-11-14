#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "graph.h"

void print_nodes(graph_t *g) {
    nodelist_t *n;
    for(n = g->nodes; n; n=n->next)
        printf("%s\n", (char *)n->node->data);
}

int main() {
    char *a = malloc(12*sizeof(char));
    char *b = malloc(12*sizeof(char));
    char *c = malloc(12*sizeof(char));
    strncpy(a, "n: lo world", 12);
    strncpy(b, "m: hello bb", 12);
    strncpy(c, "o: fsf sefs", 12);

    node_t *n = init_node((void *) a);
    node_t *m = init_node((void *) b);
    node_t *o = init_node((void *) c);
    graph_t *g = init_graph();
    graph_t *h = init_graph();
    
    add_node(g, n);
    add_node(g, m);
    add_node(h, o);

    printf("print_nodes(g):\n");
    print_nodes(g);
    printf("print_nodes(h):\n");
    print_nodes(h);

    connect_undir(m, n);
    printf("m->in = %x\n", (int) m->in);

    g = add_graphs(g, h);
    printf("print_nodes(g):\n");
    print_nodes(g);
    printf("print_nodes(h):\n");
    print_nodes(h);

    g = subtract_graphs(g, h);
//    remove_node(g, o);
    printf("print_nodes(g):\n");
    print_nodes(g);

    printf("done printing\n");

    // free everything
    free_graph(g);
    printf("g freed\n");
    free_graph(h);
    printf("h freed\n");
    free_node(m);
    printf("m freed\n");
    free_node(n);
    printf("n freed\n");
    free_node(o);
    printf("o freed\n");
    
    return 0;
}
