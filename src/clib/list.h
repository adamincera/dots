#include "graph.h"

struct list {
    struct list *next;
    struct list *previous;
    void *data;
};
typedef struct list list_t;

/* copy constructors */
list_t *string_list_copy(const list_t *src);

list_t *num_list_copy(const list_t *src);

list_t *graph_list_copy(const list_t *src);

list_t *node_list_copy(const list_t *src);

/*
list_t *dict_copy(list_t *src);

list_t *other_copy(list_t *src);
*/

/* insertion */
list_t *string_add_front(list_t *l, char *data);

list_t *num_add_front(list_t *l, float *data);

list_t *graph_add_front(list_t *l, graph_t *data);

list_t *node_add_front(list_t *l, node_t *data);

/*
list_t *dict_add_front(list_t *l, entry_t **data);

list_t *other_add_front(list_t *l, void *data);

*/
list_t *string_add_back(list_t *l, char *data);

list_t *num_add_back(list_t *l, float *data);

list_t *graph_add_back(list_t *l, graph_t *data);

list_t *node_add_back(list_t *l, node_t *data);

/*
list_t *dict_add_back(list_t *l, entry_t **data);

list_t *other_add_back(list_t *l, void *data);

*/
/* concatenation */
void string_list_concat(list_t *target, const list_t *src);

void num_list_concat(list_t *target, const list_t *src);

void graph_list_concat(list_t *target, const list_t *src);

void node_list_concat(list_t *target, const list_t *src);

/*
void dict_list_concat(list_t *target, const list_t *src);

*/
/* comparison */
int string_list_equals(const list_t *a, const list_t *b);

int num_list_equals(const list_t *a, const list_t *b);

int node_list_equals(const list_t *a, const list_t *b);

int graph_list_equals(const list_t *a, const list_t *b);

/*
int other_list_equals(const list_t *a, const list_t *b, int (*comp)(void *a, void *b));

*/

/* dequeue/pop */

void *pop(list_t *l);

/* peek */
void *peek(list_t *l);

/* freeing */
void free_list(list_t *r);

/*
void string_free_list(list_t *r);

void graph_free_list(list_t *r);

void node_free_list(list_t *r);
*/

/*
void dict_free_list(list_t *r);

void other_free_list(list_t *r);

*/
/* other */
list_t *range(int a, int b);

void *list_access(const list_t *l, int i);

void print_range(list_t *r);

void print_strings(list_t *r);

void free_list(list_t *r);

void num_index_insert(list_t *l, int i, float *a);

void string_index_insert(list_t *l, int i, char *a);

void node_index_insert(list_t *l, int i, node_t *a);

void graph_index_insert(list_t *l, int i, graph_t *a);

