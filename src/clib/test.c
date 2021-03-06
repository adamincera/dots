#include <stdio.h>
#include "dict.h"

int main() {
    list_t *l = range(0, 10);
    print_range(l);
    free_range(l);
    l = range(10, 0);
    print_range(l);
    //free_range(l);
    float a = 30;
    list_t *m = range(20, 30);
    list_t *concat = num_list_concat(l, m);
    printf("concat: ");
    print_range(concat);
    printf("concat.len = %d\n", list_len(concat));
    float minf =  num_list_min(concat);
    float maxf =  num_list_max(concat);
    printf("min: %f\nmax: %f\n", minf, maxf);
    free_range(l);
    free_range(m);
    l = range(20, 20);
    print_range(l);
    free_range(l);

    char *s1 = "hello ";
    char *s2 = "world";
    l = NULL;
    l = string_add_front(l, s2);
    l = string_add_front(l, s1);
    char *min = string_list_min(l);
    char *max = string_list_max(l);
    printf("min: %s\nmax: %s\n", min, max);
    print_strings(l);
    free_list(l);
    printf("list done\n");

    l = NULL;
    l = string_add_back(l, s1);
    l = string_add_back(l, s2);
    list_t *string_copy = string_list_copy(l);
    printf("l = ");
    print_strings(l);
    printf("string_copy = ");
    print_strings(string_copy);

    printf("l[0] = %s, l[1] = %s\n", list_access(l, 0), list_access(l, 1));

    free_list(l);

    entry_t **d = NULL; //init_dict();
    d = put_string(d, "hello", (void *) "world");
    put_string(d, "hello", (void *) "world2");
    put_string(d, "elloh", (void *) "orldw");
    put_string(d, "something else", (void *) "new value");
    min = string_dict_min(d);
    max = string_dict_max(d);
    printf("min: %s: %s\nmax: %s: %s\n", min, (char *) get_string(d, min), max, get_string(d, max));
    char *got = (char *) get_string(d, "hello");
    printf("got %s\n", got);
    got = (char *) get_string(d, "elloh");
    printf("got %s\n", got);
    got = (char *) get_string(d, "something else");
    printf("got %s\n", got);
    got = (char *) get_string(d, "world");
    printf("got %s\n", got);
    printf("d.len = %d\n", dict_len(d));

    printf("1.23 == 1.24: %d\n", float_equals(1.23,1.24));

    entry_t **nums = init_dict();
    put_num(nums, 1.23, (void *) "1.23 val");
    put_num(nums, 1.23, (void *) "1.23 second val");
    put_num(nums, 1.24, (void *) "1.24 val");
    printf("nums.len = %d\n", dict_len(nums));
    minf = *(float *) num_dict_min(nums);
    maxf = *(float *) num_dict_max(nums);
    printf("min: %f\nmax: %f\n", minf, maxf);
    got = (char *)get_num(nums, 1.23);
    printf("got %s\n", got);
    got = (char *)get_num(nums, 1.24);
    printf("got %s\n", got);
    printf("removed 1.23:\n");
    num_dict_remove(nums, 1.23);
    got = (char *)get_num(nums, 1.23);
    printf("got %s\n", got);

    node_t *n = init_node("this is a node!");
    node_t *n2 = init_node("this is a node's value!");

    entry_t **other = init_dict();
    put_other(other, n, n2);
    node_t *g = get_other(other, n);
    int size = dict_len(other);
    printf("got node containing %s, size = %d\n", g->data, size);
    node_dict_remove(other, n);
    size = dict_len(other);
    g = get_other(other, n);
    printf("got node containing %x\n, size = %d\n", (unsigned long) g, size);

	entry_t **empty = init_dict();
	int len = dict_len(empty);


    return 0;
}
