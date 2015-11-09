#!/bin/bash
# This script must reside in the "Test Suite" directory of the project
# Make sure the "dice" executable is in the "Compiler" directory

logFile=tests.log
testPath=Scanner\ Test\ Suite/
diceExecPath=../Compiler/dice
diceOption=-tendl
testExtension=.ManualTokens
pass=0
fail=0


echo "Dice Test Script Results:" >> $logFile
date >> $logFile

for testFile in "$testPath"*.dice; do

filename=$(basename "$testFile")

$diceExecPath $diceOption "$testFile" > temp_Dice_Tester

echo "==================================" >> $logFile
echo "Testing: $filename" >> $logFile
echo "" >> $logFile

diff temp_Dice_Tester "$testPath"$filename$testExtension > /dev/null

if [ $? -eq 0 ]
then
  echo "$filename passed!" >> $logFile
  ((pass++))
  
else
  echo "$filename FAILED" >> $logFile
  diff temp_Dice_Tester "$testPath"$filename$testExtension >> $logFile
  ((fail++))

fi
done

echo "==================================" >> $logFile
echo "" >> $logFile

if [ $fail -gt 0 ]
	then
	cat tests.log
fi

echo "Tests Passed: $pass"
echo "Tests Failed: $fail"
echo "Checkout tests.log (printed above)"

rm temp_Dice_Tester

exit 0