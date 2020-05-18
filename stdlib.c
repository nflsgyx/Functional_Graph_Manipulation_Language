#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

enum TYPE {
    INT = 1,
    BOOL = 2,
    FLOAT = 3,
    STRING = 4,
    VOID = 5,
    ARRAY = 6,
    LIST = 7,
    NODE = 8,
    EDGE = 9,
    GRAPH = 10
}Type;

/*
errno:
1-empty list
2-null pointer
3-unsupported type
4-different graph
5-not found
6-removing a occupied node
*/
void raise(char* s, int err){
    printf("raise error %s\n",s);
    exit(-err);
}

char *concat(char *s1, char *s2) {
	char *answer = (char *) malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(answer, s1);
	strcat(answer, s2);
	return answer;
}

typedef struct ListEle listEle;
typedef struct List list;
typedef struct Node node;
typedef struct Edge edge;
typedef struct Graph graph;

struct ListEle {
    struct ListEle *prev;
    struct ListEle *next;
    void *data;
};

struct List {
    listEle *head;
    listEle *tail;
    int length;
};

struct Node {
    int index;
    int flag;
    void *value;
    list *edges;
    graph *g;
};

struct Edge {
    node *nodeFrom;
    node *nodeTo;
    void *weight;
};

struct Graph {
    list *nodes;
};

/*
========================================
                 LIST 
========================================
*/

void print_node(node *n, int indent);

graph *init_graph();
void chain_list_element(listEle *left, listEle *right);
listEle *init_listEle(listEle *prev, listEle *next, void *data);
int list_length(list *l){
    return l->length;
}

listEle *list_get_first(list *l) {
    if(!l) {
        raise("list_get_first: null pointer", 2);
    }
    return l->head->next;
}

void *list_peek_left (list *l) {
    listEle *le;
    if(l->length == 0) {
        raise("list_peek_left: empty list", 1);
    }
    le = list_get_first(l);
	return le->data;
}

void *list_peek_right(list *l) {
    if(l->length == 0) {
        raise("list_peek_right: empty list", 1);
    }
	return l->tail->data;
}

list *init_list() {
	list *l = (list *) malloc(sizeof(list));
    listEle *dummy = init_listEle(NULL, NULL, NULL);
	l->length = 0;
    l->head = dummy;
    l->tail = dummy;
    return l;
}

listEle *init_listEle(listEle *prev, listEle *next, void *data) {
	listEle *e = (listEle *) malloc(sizeof(listEle));
	e->prev = prev;
	e->next = next;
	e->data = data;
    return e;
}

list* list_push_left(list* l, void* data){
    if(!data) {
        raise("list_push_left: null pointer", 2);
    }
    listEle* le = init_listEle(NULL, NULL, data);
    listEle* old_first = list_get_first(l);
    chain_list_element(le, old_first);
    chain_list_element(l->head, le);
    if(l->length == 0) {
        l->tail = le;
    }
    (l->length) += 1;
    return l;
}

list* list_push_right(list* l, void* data){
    if(!data) {
        raise("list_push_right: null pointer", 2);
    }
    if(!l->tail){
        raise("list_push_right: null tail", 2);
    }
    listEle* le = init_listEle(l->tail, NULL, data);
    chain_list_element(l->tail, le);
    l->tail = le;
    (l->length) += 1;
    return l;
}

void* list_pop_left(list *l) {
    void *ret_data;
    if (l->length == 0) {
        raise("list_pop_left: empty list", 1);
    }
    ret_data = list_peek_left(l);
    // l->head->next = l->head->next->next;
    chain_list_element(l->head, list_get_first(l)->next);
    l->length -= 1;
    if(l->length == 0){
        l->tail = l->head;
    }
    return ret_data;
}

void *list_pop_right(list *l) {
    void *ret_data;
    listEle* ori_tail;
    ori_tail = l->tail;
    if (l->length == 0) {
        raise("list_pop_right: empty list", 1);
    }
    ret_data = list_peek_right(l);
    l->tail = l->tail->prev;
    l->tail->next = NULL;
    l->length -= 1;
    free(ori_tail);
    return ret_data;
}

list *populate_list(list *l, void* data_head, int length, int data_size) {
   /*populate a empty list with data*/
   int i;   
   for (i = 0; i < length; i++) {
       list_push_right(l, data_head + i * data_size);
   }
}
 
void chain_list_element(listEle *left, listEle *right) {
    if (!left && !right) {
        raise("chain_list_element: null pointer", 2);
    }
    if (!left) {
        right->prev = NULL;
    } else if (!right) {
        left->next = NULL;
    } else {
        left->next = right;
        right->prev = left;
    }
}

list *list_copy(list* l1) {
    list* l2 = init_list();
    listEle *it;
    for(it = list_get_first(l1); it!=NULL; it = it->next){
        list_push_right(l2, it->data);
    }
    return l2;
}

void *list_remove_by_index(list *l, int index) {
    listEle *prev;
    listEle *curr;
    void *data;
    if(!l) {
        raise("list_remove_by_index: null pointer", 2);
    }
    if(index < 0 || list_length(l) <= index) {
        raise("list_remove_by_index: not found", 5);
    }
    prev = l->head;
    curr = list_get_first(l);
    while(index) {
        index--;
        prev = curr;
        curr = curr->next;
    }
    data = curr->data;
    chain_list_element(prev, curr->next);
    l->length--;
    free(curr);
    if(!(l->length)) {
        l->tail = l->head;
    }
    return data;
}

/*
========================================
                 NODE 
========================================
*/


void node_set_flag(node *n, int flag) {
    n->flag = flag;
}

int node_get_flag(node *n) {
    return n->flag;
}

node* init_node() {
    node *n = (node *) malloc(sizeof(node));
	n->index = -1;
    n->value = NULL;
    n->edges = init_list();
    n->g = NULL;
    n->flag = 0;
    return n;
}


int node_get_index(node* n) {
    if(!n) {
        raise("node_get_index: null pointer", 2);
    }
    return n->index;
}

void* node_get_value(node *n) {
    if(!n) {
        raise("node_get_value: null pointer", 2);
    }
    return n->value;
}

list* node_get_edges(node *n) { 
    if(!n) {
        raise("node_get_edges: null pointer", 2);
    }
    return n->edges; 
}

void node_set_index(node *n, int index) {
    if(!n) {
        raise("node_set_index: null pointer", 2);
    }
    n->index = index;
}

void node_set_value(node *n, void *value) {
    if(!n) {
        raise("node_set_value: null pointer", 2);
    }
    n->value = value;
}

char *node_to_s(node* n, enum TYPE typ){
    /* maxlen = 1000*/
    char buffer[1000];
    char* ret; 
    int len;
    if(!n) {
        raise("node_to_s: null pointer", 2);
    }
    sprintf(buffer, "%d@", n->index);
    len = strlen(buffer);
    if (typ == INT){
        sprintf(buffer+len, "%d", *((int*)(n->value)));
    } else {
        raise("node_to_s: unsupported type", 3);
    }
    len = strlen(buffer);
    ret = (char *) malloc(len + 1);
    strcpy(ret, buffer);
    return ret;
}

void assign_node(node *n, int index, void *value) {
    if(!n) {
        raise("assign_node: null pointer", 2);
    }
    n->index = index;
    n->value = value;
}

/*
========================================
                 EDGE 
========================================
*/

edge *init_edge() {
    edge *e = (edge *) malloc(sizeof(edge));
    e->nodeFrom = NULL;
    e->nodeTo   = NULL;
    e->weight   = NULL;
    return e;
}

node *edge_get_from(edge *e) {
    if(!e) {
        raise("edge_get_from: null pointer", 2);
    }
    return e->nodeFrom;
}

node *edge_get_to(edge *e) {
    if(!e) {
        raise("edge_get_to: null pointer", 2);
    }
    return e->nodeTo;
}

void *edge_get_weight(edge *e) {
    if(!e) {
        raise("edge_get_weight: null pointer", 2);
    }
    return e->weight;
}

void edge_set_from(edge *e, node *from) {
    if(!e || !from) {
        raise("edge_set_from: null pointer", 2);
    }
    e->nodeFrom = from;
}

void edge_set_to(edge *e, node *to) {
    if(!e || !to) {
        raise("edge_set_to: null pointer", 2);
    }
    e->nodeTo = to;
}

void edge_set_weight(edge *e, void *weight) {
    if(!e || !weight) {
        raise("edge_set_weight: null pointer", 2);
    }
    e->weight = weight;
}

char *edge_to_s(edge* e, enum TYPE typ){
    /* maxlen = 1000*/
    char buffer[1000];
    char* ret; 
    int len;
    if(!e) {
        raise("edge_to_s: null pointer", 2);
    }
    sprintf(buffer, "%d->", e->nodeFrom->index);
    len = strlen(buffer);
    if (typ == INT){
        sprintf(buffer+len, "%d", *((int*)(e->weight)));
    } else {
        raise("edge_to_s: unsupported type", 3);
    }
    len = strlen(buffer);
    sprintf(buffer+len, "->%d", e->nodeTo->index);
    len = strlen(buffer);
    ret = (char *) malloc(len + 1);
    strcpy(ret, buffer);
    return ret;
}

void assign_edge(edge *e, node *from, node *to, void *weight) {
    if(!e || !from || !to || !weight) {
        raise("assign_edge: null pointer", 2);
    }
    e->nodeFrom = from;
    e->nodeTo   = to;
    e->weight   = weight;
}

/*
========================================
                 GRAPH 
========================================
*/


list *graph_get_edges(graph *g);
void graph_add_node(graph *g, node* n);
int graph_add_edge_by_id(graph *g, int aid, int bid, void *value);
node *graph_get_node(graph *g, int id);

void graph_set_flags(graph *g, int flag);
graph *init_graph() {
    graph *g = (graph *) malloc(sizeof(graph));
    g->nodes = init_list();
    return g;
}

void graph_set_flags(graph *g, int flag) {
    list *nodes = list_copy(g->nodes);
    node *v;
    while(list_length(nodes) > 0) {
        v = (node *)list_pop_left(nodes);
        v->flag = flag;
    }
}

graph *graph_copy(graph *g1) {
    graph *g2 = init_graph();
    list *edges1 = graph_get_edges(g1);
    listEle *it; // it->head->next

    node *n1, *n2;
    edge *e1, *e2;
    int from_idx, to_idx;

    for(it = list_get_first(g1->nodes); it!=NULL; it = it->next) {
        n1 = (node*)(it->data);
        n2 = init_node();
        n2->index = n1->index;
        n2->value = n1->value;
        n2->edges = init_list();
        graph_add_node(g2, n2);
    }
    for(it = list_get_first(edges1); it != NULL; it = it->next) {
        e1 = (edge*)(it->data);
        from_idx = e1->nodeFrom->index;
        to_idx = e1->nodeTo->index;
        graph_add_edge_by_id(g2, from_idx, to_idx, e1->weight);
    }
    return g2;
}

list *graph_get_nodes(graph *g) {
    return g->nodes;
} 

list *graph_get_edges(graph *g) {
   listEle *n_le = list_get_first(g->nodes);
   listEle *e_le;
   node* n;
   edge* e;
   list* edges = init_list();
   if(!g) {
       raise("graph_get_edges: null pointer", 2);
   }
   while(n_le) {
       n = (node*)(n_le->data);
       e_le = list_get_first(n->edges);
       while (e_le) {
           e = (edge*)(e_le->data);
           list_push_right(edges, e);
           e_le = e_le->next;
       }
       n_le = n_le->next;
   }
   return edges;
}

void graph_add_node(graph *g, node* n) {
    if(!g || !n) {
        raise("graph_add_node: null pointer", 2);
    }
    if(n->g && n->g!=g) {
        raise("graph_add_node: different graph", 4);
    }
    n->g = g;
    list_push_right(g->nodes, (void *)n);
}

// void get_indent(int k) {
//     int i = 0;
//     for (int i = 0; i < k; i++) {
//         printf("  ");
//     }
// }

// void print_node(node *n, int indent) {
//     get_indent(indent);
//     printf("node: index: %d, value addr: %p\n", n->index, n->value);
// }

// void print_edge(char *prefix, edge *e, int indent) {
//     get_indent(indent);
//     printf("%s: edge[%p]\n", prefix, e);
//     print_node(e->nodeFrom, indent + 1);
//     print_node(e->nodeTo, indent + 1);
//     get_indent(indent + 1);
//     printf("weight: [%p]\n", e->weight);
// }

char *graph_to_s(graph* g, enum TYPE n_typ, enum TYPE e_typ){
    /* maxlen = 1000*/
    char n_buffer[1000];
    char e_buffer[1000];
    char *ret; 
    int n_len=0;
    int e_len=0;
    int is_first = 1;
    listEle *ptr;
    list *edges;
    listEle *e_ptr;
    if(!g) {
        raise("graph_to_s: null pointer", 2);
    }
    n_buffer[0] = '\0';
    e_buffer[0] = '\0';
    ptr = list_get_first(g->nodes);
    edges = graph_get_edges(g);
    e_ptr = list_get_first(edges);
    while(ptr){
        if(!is_first) {
            sprintf(n_buffer+n_len, ", ");
            n_len+=2;
        }
        is_first = 0;
        strcpy(n_buffer+n_len, node_to_s((node*)(ptr->data), n_typ));
        n_len = strlen(n_buffer);
        ptr = ptr->next;
    }
    sprintf(n_buffer+n_len, "\n");
    n_len = strlen(n_buffer);
    is_first = 1;
    while(e_ptr){
        if(!is_first) {
            sprintf(e_buffer+e_len, ", ");
            e_len+=2;
        }
        is_first = 0;
        strcpy(e_buffer+e_len, edge_to_s((edge*)(e_ptr->data), e_typ));
        e_len = strlen(e_buffer);
        e_ptr = e_ptr->next;
    }
    ret = (char*) malloc(n_len+e_len+1);
    strcpy(ret, n_buffer);
    strcpy(ret+n_len, e_buffer);
    return ret;
}

int graph_add_edge(graph *g, edge* e) {
    if(!g || !e) {
        raise("graph_add_edge: null pointer", 2);
    }
    if (!e->nodeFrom->g || e->nodeFrom->g != g
        || !e->nodeTo->g || e->nodeTo->g != g) {
        raise("graph_add_edge: different graph", 4);
    }
    list_push_right(e->nodeFrom->edges, e);
    return 0;
}

int graph_add_edge_by_node(graph *g, node *a, node *b, void *value) {
    if(!g || !a || !b || !value) {
        raise("graph_add_edge_by_node: null pointer", 2);
    }
    edge *e = init_edge();
    if (!a->g || a->g != g || !b->g || b->g != g) {
        raise("graph_add_edge_by_node: different graph", 4);
    }
    assign_edge(e, a, b, value);
    list_push_right(a->edges, e);
    return 0;
}

node *graph_get_node(graph *g, int id) {
    node *n;
    listEle *curr;
    if(!g) {
        raise("graph_get_node: null pointer", 2);
    }
    curr = list_get_first(g->nodes);
    if (curr == NULL) {
        return NULL;
    }
    while (curr != NULL) {
        if ((((node*)(curr->data))->index) == id) {
            return (node*)(curr->data);
        }
        curr = curr->next;
    }
    return NULL;
}

int graph_add_edge_by_id(graph *g, int aid, int bid, void *value) {
    node *a, *b;
    if(!g || !value) {
        raise("graph_add_edge_by_id: null pointer", 2);
    }
    a = graph_get_node(g, aid);
    b = graph_get_node(g, bid);
    if (a == NULL || b == NULL) {
        raise("add_edge_by_id: null pointer", 2);
    }
    return graph_add_edge_by_node(g, a, b, value);
}

void list_remove(list *l, void *data) {
    listEle *prev;
    listEle *curr;
    void *list_data;
    if(!l || !data) {
        raise("list_remove: null pointer", 2);
    }
    prev = l->head;
    curr = list_get_first(l);
    while(curr) {
        list_data = curr->data;
        if(list_data = data) {
            chain_list_element(prev, curr->next);
            l->length--;
            free(curr);
            if(!(l->length)) {
                l->tail = l->head;
            }
            return;
        }
        prev = curr;
        curr = curr->next;
    }
    raise("list_remove: not found", 5);
}

void graph_remove_node(graph *g, node *n) {
    list *l;
    if(!g || !n) {
        raise("graph_remove_node: null pointer", 2);
    }
    if(list_length(node_get_edges(n))) {
        raise("graph_remove_node: removing a occupied node", 6);
    }
    // Can delete node that has edges (to node)
    l = graph_get_nodes(g);
    list_remove(l, n);
}

void graph_remove_edge(graph *g, edge *e) {
    node *n;
    list *l;
    if(!g || !e) {
        raise("graph_remove_edge: null pointer", 2);
    }
    n = e->nodeFrom;
    l = node_get_edges(n);
    list_remove(l, e);
}