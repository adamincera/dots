struct node;
struct edgelist;
typedef struct node node_t;
typedef struct edgelist edgelist_t;

struct node {
    void *data;
    edgelist_t *in;
    edgelist_t *out;
};

struct edgelist {
    node_t *node;
    float weight;
    edgelist_t *next;
    edgelist_t *previous;
};

/* initialize a new node that contains *data */
node_t *init_node(void *data);

/* compares node data */
int node_compare(node_t *a, node_t *b, int (* comp) (void *a, void *b));

/* create undirected edge of weight 0 */
void connect_undir(node_t *a, node_t *b);

/* create weighted undirected edge */
void connect_undir_weighted(node_t *a, node_t *b, float weight);

/* create directed edge of weight 0 */
void connect_dir(node_t *src, node_t *dst);

/* create weighted directed edge */
void connect_dir_weighted(node_t *src, node_t *dst, float weight);

/* remove directed edge from src to dst */
void remove_dir_edge(node_t *src, node_t *dst);

/* remove undirected edge between a and b */
void remove_undir_edge(node_t *a, node_t *b);

/* deallocate node */
void free_node(node_t *n);
