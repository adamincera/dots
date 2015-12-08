#define TABLE_SIZE 1024

struct entry {
    void *key;
    void *value;
    struct entry *next;
};

typedef struct entry entry_t;

entry_t **init_dict();

void *get_string(entry_t **table, char *key);

void *get_num(entry_t **table, float key);

void *get_other(entry_t **table, void *key);

void put_string(entry_t **table, char *key, void *value);

void put_num(entry_t **table, float key, void *value);

void put_other(entry_t **table, void *key, void *value);
