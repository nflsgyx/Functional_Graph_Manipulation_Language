function foo3(int a, int b) int{
    int c = a + b;
    return c;
}

float a = 6.7;


/*
function arg arg typ mismatch
*/

/* foo3(a, 2); */




/*
function arg num mismatch
*/

/* foo3(6, 2, 7); */



/*
function arg return typ mismatch
*/

/* foo3(a, 2); */
float s = foo3(1, 2);