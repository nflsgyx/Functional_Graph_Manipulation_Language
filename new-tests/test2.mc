/*
function as arg
*/

function printThreeTimes(int a, func(int, int; int) foo) void{
    print(foo(a, 2*a));
}

function sum(int a, int b) int{
    int c = a + b;
    return c;
}

printThreeTimes(5, sum);


/*
function as return type
*/

function foo(node<int> a, graph<int, int> g) func(int;int) {
    int x = a.get_index();
    print(x);
    return function (int n) int {
        return x + n;
    };
}

node<int> a = 100@100;
graph<int,int> b;
b.add_node(a);

func(int; int) f = foo(a, b);
print(f(10));



