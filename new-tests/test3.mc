/*
scoping
*/


int q = 7;

if (q < 10) {
    node<int> q = 1@2;

    function foo(int a, int b) void {
        print(q.get_index());
        print(a+b);
        return;
    }

    foo(1, 2);

} else {

    list<float> q = [2.5, 2.4];

    function foo(float a) float{
        printf(q.pop_left());
        float q = 5.0;
        return a + q;
    }

    printf(foo(4.0));
}

print(q);