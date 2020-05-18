/*

program as stmt list

variable initialization and assignment in one line

function initialization and assignment

*/

int p;
p = 7;

int q = 99;
string a = "abc";

function foo(float a, list<int> b) void {
    printf(a);
    b.push_right(9);
    print(b.length());
}

list<int> l = [2, 3, 5, 7];
foo(3.5, l);


if (q < 10) {
    prints("q < 10");
} else {
    prints("q >= 10");
}