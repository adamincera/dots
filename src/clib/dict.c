#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "dict.h"

#define EPSILON 0.000000001

entry_t **init_dict() {
    entry_t **table = (entry_t **) calloc(TABLE_SIZE, sizeof(entry_t *));
    return table;
}

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
    nodelist_t *temp;
    for(temp = g->nodes; temp; temp = temp->next) {
        ret += ((unsigned int) temp->node >> 3);
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
    printf("here. temp = %o = table[%d]\n", temp, k);
        table[k] = (entry_t *) malloc(sizeof(entry_t));
        table[k]->next = NULL;
        table[k]->value = value;
        table[k]->key = graph_copy(key);
    } else {
    printf("here. temp = %o = table[%d]\n", temp, k);
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

void dict_remove(entry_t *table, entry_t *target) {
    while(table) {
        if(table->next == target) {
            table->next = target->next;
            free(target);
            return;
        }
        table = table->next;
    }
}
