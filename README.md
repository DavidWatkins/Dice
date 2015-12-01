# Dice
Java, but worse

##Requirements
- The test environment is Ubuntu 15.10 running within Virtualbox
- The following ubuntu packages were installed:
  m4 clang-3.7 clang-3.7-doc libclang-common-3.7-dev libclang-3.7-dev libclang1-3.7 libclang1-3.7-dbg libllvm-3.7-ocaml-dev libllvm3.7 libllvm3.7-dbg lldb-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-doc llvm-3.7-examples llvm-3.7-runtime clang-modernize-3.7 clang-format-3.7 python-clang-3.7 lldb-3.7-dev liblldb-3.7-dbg opam llvm-runtime
- The following opam packages were installed:
  core batteries llvm yojson


##How to run the compiler
- Make sure you are running Ubuntu 15.10 or equivalent
- clone the repo:
```bash
git clone https://github.com/DavidWatkins/Dice.git
```
- Then install the required packages
```bash
sudo apt-get install m4 clang-3.7 clang-3.7-doc libclang-common-3.7-dev libclang-3.7-dev libclang1-3.7 libclang1-3.7-dbg libllvm-3.7-ocaml-dev libllvm3.7 libllvm3.7-dbg lldb-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-doc llvm-3.7-examples llvm-3.7-runtime clang-modernize-3.7 clang-format-3.7 python-clang-3.7 lldb-3.7-dev liblldb-3.7-dbg opam llvm-runtime
```
- Then initialize opam in your home directory
```bash
opam init
opam switch 4.02.1
eval $(opam config env)
opam install core batteries llvm yojson
```
- Then go into the Compiler directory and run the build script
```bash
cd Dice/Compiler
./build.sh
./dice -c input.dice > test.ll
lli test.ll
```

##To test the compiler
- cd into Test Suite and run tester.sh after having compiled the compiler
```bash
cd Dice/'Test Suite'
./tester.sh -c
```

###If you get an error: "error: expected value token" from lli
This means your version of lli is probably incorrect. Running "lli --version" should return 3.7, but if it doesn't:
- Run the following commands to remove the symlink of lli in your /usr/bin directory
```bash
sudo rm \usr\bin\lli
ln -s /usr/lib/llvm-3.7/bin/lli /usr/bin/lli
```
- Now you should be able to use lli with the outputted llvm code from dice

