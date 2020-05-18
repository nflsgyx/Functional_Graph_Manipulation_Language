../gmail.native ./test1.mc > test.ll
llc -relocation-model=pic test.ll > test.s
cc -o test.exe test.s ../stdlib.o -lm
./test.exe > test1.out


../gmail.native ./test2.mc > test.ll
llc -relocation-model=pic test.ll > test.s
cc -o test.exe test.s ../stdlib.o -lm
./test.exe > test2.out

../gmail.native ./test3.mc > test.ll
llc -relocation-model=pic test.ll > test.s
cc -o test.exe test.s ../stdlib.o -lm
./test.exe > test3.out


../gmail.native ./test4.mc > test.ll
llc -relocation-model=pic test.ll > test.s
cc -o test.exe test.s ../stdlib.o -lm
./test.exe > test4.out

rm test.ll test.exe test.s