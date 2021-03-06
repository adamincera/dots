#include "list.h"
#define TABLE_SIZE 256

entry_t **init_dict();

void *get_string(entry_t **table, char *key);

void *get_num(entry_t **table, float key);

void *get_graph(entry_t **table, graph_t *g);

void *get_other(entry_t **table, void *key);

void *get_node(entry_t **table, node_t *n);

entry_t **put_string(entry_t **table, char *key, void *value);

entry_t **put_num(entry_t **table, float key, void *value);

entry_t **put_graph(entry_t **table, graph_t *g, void *value);

entry_t **put_node(entry_t **table, node_t *n, void *value);

entry_t **put_other(entry_t **table, void *key, void *value);

int float_equals(float a, float b);

void num_dict_remove(entry_t **table, float key);

void string_dict_remove(entry_t **table, char *key);

void node_dict_remove(entry_t **table, node_t *key);

void graph_dict_remove(entry_t **table, graph_t *key);

entry_t **dict_copy(entry_t **table);

void *num_dict_min(entry_t **table);

void *num_dict_max(entry_t **table);

void *string_dict_min(entry_t **table);

void *string_dict_max(entry_t **table);

int dict_len(entry_t **table);
