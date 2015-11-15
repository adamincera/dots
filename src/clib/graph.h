#include "node.h"
typedef struct nodelist nodelist_t;
struct nodelist {
    node_t *node;
    nodelist_t *next;
    nodelist_t *previous;
};

typedef struct graph graph_t;
struct graph {
    nodelist_t *nodes;
    int count;
};

graph_t *init_graph();

void free_graph(graph_t * graph);

int contains(graph_t *graph, void *data, int (* comp)(void *a, void *b));

void add_node(graph_t *graph, node_t *node);

int remove_node(graph_t *graph, node_t *node);

graph_t *add_graphs(graph_t *a, graph_t *b);

graph_t *subtract_graphs(graph_t *left, graph_t *right);
