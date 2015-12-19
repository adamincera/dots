#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "dict.h"

#define EPSILON 0.000000001

entry_t **init_dict() {
    entry_t **table = (entry_t **) calloc(TABLE_SIZE, sizeof(entry_t *));
    return table;
}

/*
   entry_t **dict_copy(entry_t **src) {
   entry_t **dst = init_dict();
   int i;
   entry_t *temp;
   for(i = 0; i < TABLE_SIZE; i++)
   for(temp = src[i]; temp; temp = temp->next) {
 */

int float_equals(float a, float b) {
    return fabsf(a - b) < EPSILON * a;
}

static int hash_string(char *key) {
    unsigned int h = 0;
    int i = 0;
    while(key[i])
        h += key[i++];
    h %= TABLE_SIZE;
    return h;
}

static int hash_num(float key) {
    return (unsigned int) key % TABLE_SIZE;
}

static int hash_graph(graph_t *g) {
    int ret = 0;
    list_t *temp;
    for(temp = g->nodes; temp; temp = temp->next) {
        ret += ((unsigned int) temp->data >> 3);
    }
    ret %= TABLE_SIZE;
    return ret;
}

static int hash_other(void *key) {
    return ((unsigned int) key) % TABLE_SIZE;
}

void *get_string(entry_t **table, char *key) {
    int k = hash_string(key);
    entry_t *temp = table[k];
    while(temp) {
        if(!strcmp((char *) temp->key, key)) {
            return temp->value;
        }
        temp = temp->next;
    }
    return 0;
}

void *get_num(entry_t **table, float key) {

    int k = hash_num(key);
    entry_t *temp = table[k];
    while(temp) {
        if(float_equals(*(float *) temp->key, key)) {
            return temp->value;
        }
        temp = temp->next;
    }
    return 0;
}

void *get_graph(entry_t **table, graph_t *key) {
    int k = hash_graph(key);
    entry_t *temp = table[k];
    while(temp) {
        if(graph_equals((graph_t *) temp->key, key)) {
            return temp->value;
        }
        temp = temp->next;
    }
    return 0;
}

void *get_node(entry_t **table, node_t *n) {
    return get_other(table, (void *) n);
}

void *get_other(entry_t **table, void *key) {
    int k = hash_other(key);
    entry_t *temp = table[k];
    while(temp) {
        if(temp->key == key) {
            return temp->value;
        }
        temp = temp->next;
    }
    return 0;
}

void put_string(entry_t **table, char *key, void *value) {
    int k = hash_string(key);
    entry_t *temp = table[k];
    if(!temp) {
        table[k] = (entry_t *) malloc(sizeof(entry_t));
        table[k]->next = NULL;
        table[k]->value = value;
        table[k]->key = key;
    } else {
        while(temp->next) {
            if(!strcmp((char *) temp->key, key)) {
                temp->value = value;
                return;
            }
            temp = temp->next;
        }
        if(!strcmp((char *) temp->key, key)) {
            temp->value = value;
            return;
        }
        temp->next = (entry_t *) malloc(sizeof(entry_t));
        temp = temp->next;
        temp->key = key;
        temp->value = value;
        temp->next = NULL;
    }
}

void put_num(entry_t **table, float key, void *value) {
    int k = hash_num(key);
    entry_t *temp = table[k];
    if(!temp) {
        table[k] = (entry_t *) malloc(sizeof(entry_t));
        table[k]->next = NULL;
        table[k]->value = value;
        table[k]->key = malloc(sizeof(float));
        *((float *) table[k]->key) = key;
    } else {
        while(temp->next) {
            if(float_equals(*(float *) temp->key, key)) {
                temp->value = value;
                return;
            }
            temp = temp->next;
        }
        if(float_equals(*(float *) temp->key, key)) {
            temp->value = value;
            return;
        }
        temp->next = (entry_t *) malloc(sizeof(entry_t));
        temp = temp->next;
        temp->key = malloc(sizeof(float));
        *((float *) temp->key) = key;
        temp->value = value;
        temp->next = NULL;
    }
}

void put_graph(entry_t **table, graph_t *key, void *value) {
    int k = hash_graph(key);
    entry_t *temp = table[k];
    if(!temp) {
        table[k] = (entry_t *) malloc(sizeof(entry_t));
        table[k]->next = NULL;
        table[k]->value = value;
        table[k]->key = graph_copy(key);
    } else {
        while(temp->next) {
            if(graph_equals((graph_t *) temp->key, key)) {
                temp->value = value;
                return;
            }
            temp = temp->next;
        }
        if(graph_equals((graph_t *) temp->key, key)) {
            temp->value = value;
            return;
        }
        temp->next = (entry_t *) malloc(sizeof(entry_t));
        temp = temp->next;
        temp->key = graph_copy(key);
        temp->value = value;
        temp->next = NULL;
    }
}

void put_other(entry_t **table, void *key, void *value) {
    int k = hash_other(key);
    entry_t *temp = table[k];
    if(!temp) {
        table[k] = (entry_t *) malloc(sizeof(entry_t));
        table[k]->next = NULL;
        table[k]->value = value;
        table[k]->key = key;
    } else {
        while(temp->next) {
            if(temp->key == key) {
                temp->value = value;
                return;
            }
            temp = temp->next;
        }
        if(temp->key == key) {
            temp->value = value;
            return;
        }
        temp->next = (entry_t *) malloc(sizeof(entry_t));
        temp = temp->next;
        temp->key = key;
        temp->value = value;
        temp->next = NULL;
    }
}

void put_node(entry_t **table, node_t *key, void *value) {
    put_other(table, (void *) key, value); 
}

static void dict_remove(entry_t **table, void *key, int (*comp)(void *a, void *b)) {
    entry_t *row = *table;

    if(!table || !row)
        return;

    if(comp) {
        if(comp(key, row->key)) {
            *table = row->next;;
            free(row);
            return;
        }
    } else {
        if(key == row->key) {
            *table = row->next;;
            free(row);
            return;
        }
    }


    for(; row->next; row = row->next) {
        if(comp) {
            if(comp(key, row->next->key)) {
                row->next = row->next->next;
                free(row->next);
                return;
            }
        } else {
            if(key == row->next->key) {
                row->next = row->next->next;
                free(row->next);
                return;
            }
        }

    }
}

static int void_float_equals(void *a, void *b) {
    return float_equals(*(float *) a, *(float *) b);
}

void num_dict_remove(entry_t **table, float key) {
    int k = hash_num(key);
    dict_remove(table + k, (void *) &key, void_float_equals);
}

static int str_equals(void *a, void *b) {
    return !strcmp((char *) a, (char *) b);
}

void string_dict_remove(entry_t **table, char *key) {
    int k = hash_string(key);
    dict_remove(table + k, (void *) key, str_equals);
}

void node_dict_remove(entry_t **table, node_t *key) {
    int k = hash_other(key);
    dict_remove(table + k, (void *) key, NULL);
}

static int void_graph_equals(void *a, void *b) {
    return graph_equals((graph_t *) a, (graph_t *) b);
}

void graph_dict_remove(entry_t **table, graph_t *key) {
    int k = hash_graph(key);
    dict_remove(table + k, (void *) key, void_graph_equals);
}

void *num_dict_min(entry_t **table) {
    int min = INT_MAX;
    void *ret = NULL;
    int i;
    entry_t *temp;
    for(i = 0; i < TABLE_SIZE; i++) 
        for(temp = table[i]; temp; temp = temp->next) {
            if(*(float *) temp->value < min) {
                min = *(float *) temp->value;
                ret = temp->key;
            }
        }
    return ret;
}

void *num_dict_max(entry_t **table) {
    int max = INT_MIN;
    void *ret = NULL;
    int i;
    entry_t *temp;
    for(i = 0; i < TABLE_SIZE; i++) 
        for(temp = table[i]; temp; temp = temp->next) {
            if(*(float *) temp->value > max) {
                max = *(float *) temp->value;
                ret = temp->key;
            }
        }
    return ret;
}

void *string_dict_min(entry_t **table) {
    char *min = NULL;
    void *ret = NULL;
    int i;
    entry_t *temp;
    for(i = 0; i < TABLE_SIZE; i++) 
        for(temp = table[i]; temp; temp = temp->next) {
            if(!min) {
                min = (char *) temp->value;
                ret = temp->key;
            } else if(strcmp((char *) temp->value, min) < 0) {
                min = (char *) temp->value;
                ret = temp->key;
            }
        }
    return ret;
}

void *string_dict_max(entry_t **table) {
    char *max = NULL;
    void *ret = NULL;
    int i;
    entry_t *temp;
    for(i = 0; i < TABLE_SIZE; i++) 
        for(temp = table[i]; temp; temp = temp->next) {
            if(!max) {
                max = (char *) temp->value;
                ret = temp->key;
            } else if(strcmp((char *) temp->value, max) > 0) {
                max = (char *) temp->value;
                ret = temp->key;
            }
        }
    return ret;
}

int len(entry_t **table) {
    int i;
    entry_t *temp;
    int len = 0;
    for(i = 0; i < TABLE_SIZE; i++)
        for(temp = table[i]; temp; temp = temp->next)
            len++;
    return len;
}
