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

    float q = 1.2;

    function foo(float a) float{
        return a + q;
    }

    /*
    printf(q);
    */

    printf(foo(q));
}


/*
wrong global var typ
*/

/*
printf(q);
*/


/*
call local func
*/

/*
foo(q);
*/







