struct list {
    struct list *next;
    struct list *previous;
    void *data;
};
typedef struct list list_t;

list_t *range(int a, int b);

void free_range(list_t *r);

void print_range(list_t *r);
