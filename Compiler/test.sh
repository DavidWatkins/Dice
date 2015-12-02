./build.sh
TESTNAME=test-arith1
TESTPATH=
./dice -f ../Test\ Suite/Compiler_Test_Suite/$TESTNAME.dice
lli $TESTNAME.ll
./dice -p ../Test\ Suite/Compiler_Test_Suite/$TESTNAME.dice
cat $TESTNAME.ll
rm $TESTNAME.ll