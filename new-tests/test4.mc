/*
builtinFcns
*/

node<int> n1 = 1@11;
node<int> n2 = 2@22;
graph<int, int> g;

g.add_node(n1);
g.add_node(n2);
g.add_edge_by_id(1,2,5);
int len = n1.get_edges().length();
print(len);

edge<int> e = n1->33->n2;
prints(e.to_s());

g.remove_edge(e);

g.set_flags(100);

node<int> n3 = 1@11;
node<int> n4 = 2@22;

g.add_node(n3);
g.add_node(n4);
g.add_edge_by_node(n3,n4,33);

list<int> l1 = [1,2,3];
list<int> l2 = l1.copy();