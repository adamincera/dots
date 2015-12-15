#include <stdio.h>
#include "list.h"
#include "dict.h"

int main() {
    list_t *l = range(0, 10);
    print_range(l);
    free_range(l);
    l = range(10, 0);
    print_range(l);
    free_range(l);
    l = range(20, 30);
    print_range(l);
    free_range(l);
    l = range(20, 20);
    print_range(l);
    free_range(l);

    char *s1 = "hello ";
    char *s2 = "world";
    l = NULL;
    l = add_front(l, s2);
    l = add_front(l, s1);
    print_strings(l);
    free_list(l);
    printf("list done\n");

    l = NULL;
    l = add_back(l, s1);
    l = add_back(l, s2);
    print_strings(l);
    free_list(l);

    entry_t **d = init_dict();
    put_string(d, "hello", (void *) "world");
    put_string(d, "hello", (void *) "world2");
    put_string(d, "elloh", (void *) "orldw");
    put_string(d, "something else", (void *) "new value");
    char *got = (char *) get_string(d, "hello");
    printf("got %s\n", got);
    got = (char *) get_string(d, "elloh");
    printf("got %s\n", got);
    got = (char *) get_string(d, "something else");
    printf("got %s\n", got);
    got = (char *) get_string(d, "world");
    printf("got %s\n", got);

    printf("1.23 == 1.24: %d\n", float_equals(1.23,1.24));

    entry_t **nums = init_dict();
    put_num(nums, 1.23, (void *) "1.23 val");
    put_num(nums, 1.23, (void *) "1.23 second val");
    put_num(nums, 1.24, (void *) "1.24 val");
    got = (char *)get_num(nums, 1.23);
    printf("got %s\n", got);
    got = (char *)get_num(nums, 1.24);
    printf("got %s\n", got);

    node_t *n = init_node("this is a node!");
    node_t *n2 = init_node("this is a node's value!");

    entry_t **other = init_dict();
    put_other(other, n, n2);
    node_t *g = get_other(other, n);
    printf("got node containing %s\n", g->data);

    return 0;
}
