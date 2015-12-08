struct list {
    struct list *next;
    struct list *previous;
    void *data;
};
typedef struct list list_t;

list_t *add_front(list_t *l, void *data);

list_t *add_back(list_t *l, void *data);

list_t *range(int a, int b);

void free_range(list_t *r);

void print_range(list_t *r);

void print_strings(list_t *r);

void free_list(list_t *r);
