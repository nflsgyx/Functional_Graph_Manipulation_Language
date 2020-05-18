# GMAIL: Graph Manipulation And Implementation Language with First-Class Function

## Participants

- Ni Chang (Tester)
- Siwei Chen (Manager)
- Yuxuan Guo (System Architect)
- Haotang Liu (Language Guru)
- Jinhai Su (System Architect)

Useful command lines:

docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt

ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 gmail.native


./gmail.native ./new-tests/test1.mc > test.ll

llc -relocation-model=pic test.ll > test.s

cc -o test.exe test.s stdlib.o -lm

./test.exe

