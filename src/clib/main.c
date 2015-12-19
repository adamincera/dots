#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dict.h"

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
    printf("%o\n", (int) a);
    printf("%o\n", (int) b);
    printf("%o\n", (int) c);

    add_node(g, n);
    add_node(g, m);
    add_node(h, o);

    graph_t *sum = node_plus_node(n, m);

    printf("print_nodes(g):\n");
    print_nodes(g);
    printf("print_nodes(h):\n");
    print_nodes(h);
    printf("print_nodes(sum):\n");
    print_nodes(sum);

    connect_dir_weighted(m, n, 0.4);
    connect_dir_weighted(n, m, 0.8);
    printf("m --[%g]-- n\n", *(float *) get_node(m->out, n));
    printf("n --[%g]-- m\n", *(float *) get_node(n->out, m));

    /*
       printf("%s -> %s\n", (char *)m->data, (char *)m->out->node->data);
       printf("%s -> %s\n", (char *)n->data, (char *)n->out->node->data);
     */

    entry_t **d = init_dict();
    put_graph(d, g, (void *) "this is graph g's data");
    printf("here:\n");
    put_graph(d, h, "this is graph h");

    printf("%s\n", get_graph(d, g));
    printf("%s\n", get_graph(d, h));

    remove_undir_edge(m, n);
    /*
       printf("%s -> %x\n", (char *)m->data, (int)m->out);
       printf("%s -> %x\n", (char *)n->data, (int)n->out);
     */

    plus_equals(g, h);
    printf("print_nodes(g):\n");
    print_nodes(g);
    printf("print_nodes(h):\n");
    print_nodes(h);
    graph_t *g_copy = graph_copy(g);
    int x = graph_equals(g, g_copy);
    printf("x = %d\n", x);
    x = graph_equals(h, g_copy);
    printf("x = %d\n", x);
    printf("print_nodes(copy):\n");
    print_nodes(g_copy);
    graph_t *diff;

    diff = minus(g, h);
    remove_node(g, o);
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
