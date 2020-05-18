list<node> BFS(graph g) {
	list<int> visited = [];
	list<node> result = [];
	list<node> q = [];
list<node> nodes = g.get_all_nodes();

	for(node n: nodes){
		if(!visited.contains(n.idx)) {
            q.push_right(s);
            visited.push_right(u.idx);
            result.push_right(u);
			
            while (q.size() != 0) {
                node u = q.pop_left();
                list<edge> edges = u.get_edges();
                for(edge e: edges) {
                    node v = e.to;
                    if(!visited.contains(v.idx)) {
                        q.push_right(v);
                        visited.push_right(v.idx);
                        result.push_right(v);
                    }
                }
            }
        }
    }
    return result;
}

int main() {
	graph g = {0(100) + 1(101) + 2(102) + 3(103), $0->1$ + 0->2 + 2->3};
	list<node> result = BFS(g);
	
	return 0;
}
