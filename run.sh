./gmail.native ./new-tests/test2.mc > test.ll
llc -relocation-model=pic test.ll > test.s
cc -o test.exe test.s stdlib.o -lm
./test.exe