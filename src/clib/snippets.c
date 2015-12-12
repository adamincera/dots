/* for node in graph */
node_t *node;
for(node = graph->nodes; node; node = node->next) {
    /* loop body */
}

/* for node in list */
list_t *node;
for(node = list; node; node = node->next) {
    /* loop body */
}

/* string s = arg */
char *s = arg;

/* num n = arg */
float n = arg;

/* node x */
node_t *x = init_node("");

/* node y(str) */
node_t *x = init_node(string);

/* graph g1; */
graph_t *g1 = init_graph();

/* graph g2 = g1; */
graph_t *g2 = copy(g1);

/* graph g3 = {
       x
       y
   };
   */
graph_t *g3 = init_graph();
add_node(g3, x);
add_node(g3, y);

/* function declarations:
   def return_type function_name(num arg1, node arg2) {
       return return_type;
   }
   */

return_type function_name(float arg1, node_t *arg2) {
    return return_type;
}

/* node1 == node 2 */
node_compare(node1, node2);

/* node1 != node2 */
!node_compare(node1, node2);

/* while statement */
while(statement) {
    /* loop body */
}

/* if condition */
if(condition) {

    /* else if */
} else if {

    /* else */
} else {
}

/* x -- y */
connect_undir(x, y);

/* x --> y */
connect_dir(x, y);

/* x --[n] y */
connect_undir(x, y, n);

/* x -->[n] y */
connect_dir(x, y, n);

/* x [m]--[n] y */
connect_dir(x, y, n);
connect_dir(y, x, m);

/* g1 = g2 + g3 */
g1 = plus(g2, g3);

/* g1 += g2 */
plus_equals(g1, g2);

/* list<num> l = [1, 2, 3] */
list_t *l = NULL;
int *i;

i = (int *) malloc(sizeof(int));
*i = 1;
l = add_back(l, i);
i = (int *) malloc(sizeof(int));
*i = 2;
l = add_back(l, i);
i = (int *) malloc(sizeof(int));
*i = 3;
l = add_back(l, i);

/** dict initialization **/
/* dict<type, type> d; */
entry_t **d = init_dict();

/** dict insertion **/
/* d["literal"] = something */
put_string(d, "literal", (void *) &something);

/* d[1.23] = something */
put_num(d, 1.23, (void *) &something);

/* d[_node] = something */
put_other(d, (void *) &_node, (void *) &something);

/** dict access **/
/* something = d["key"]; */
something = *(type *) get_string(d, "key");

/* something = d[1.23]; */
something = *(type *) get_num(d, 1.23);

/* something = d[_node]; */
something = *(type *) get_other(d, &_node);

/* for(key in d) */
int i;
entry_t *temp;
void *key;
for(i = 0; i < TABLE_SIZE; i++) {
    for(temp = d[i]; temp; temp = temp->next) {
        key = temp->key;

    }
}
