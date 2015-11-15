# Dice
Java, but worse

##Requirements
- The test environment is Ubuntu 15.10 running within Virtualbox
- The following ubuntu packages were installed:
  m4 clang-3.7 clang-3.7-doc libclang-common-3.7-dev libclang-3.7-dev libclang1-3.7 libclang1-3.7-dbg libllvm-3.7-ocaml-dev libllvm3.7 libllvm3.7-dbg lldb-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-doc llvm-3.7-examples llvm-3.7-runtime clang-modernize-3.7 clang-format-3.7 python-clang-3.7 lldb-3.7-dev liblldb-3.7-dbg opam
- The following opam packages were installed:
  core batteries llvm


##How to run the compiler
- Make sure you are running Ubuntu 15.10 or equivalent
- clone the repo:
```bash
git clone https://github.com/DavidWatkins/Dice.git
```
- Then install the required packages
```bash
sudo apt-get install m4 clang-3.7 clang-3.7-doc libclang-common-3.7-dev libclang-3.7-dev libclang1-3.7 libclang1-3.7-dbg libllvm-3.7-ocaml-dev libllvm3.7 libllvm3.7-dbg lldb-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-doc llvm-3.7-examples llvm-3.7-runtime clang-modernize-3.7 clang-format-3.7 python-clang-3.7 lldb-3.7-dev liblldb-3.7-dbg opam
```
- Then initialize opam in your home directory
```bash
opam init
opam switch 4.02.1
eval $(opam config env)
opam install core batteries llvm
```
- Then go into the Compiler directory and run the build script
```bash
cd Dice/Compiler
./build.sh
./dice -c input.dice
```

##More info to come