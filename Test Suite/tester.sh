#!/bin/bash
# This script must reside in the "Test Suite" directory of the project
# Make sure the "dice" executable is in the "Compiler" directory

diceExecPath=../Compiler/dice
testOption=$1 #stores the test flag since functions can't see the $1
vFlag=$2 #stores the -v flag since functions can't see it with $2
pass=0
fail=0
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
errorFile=errors.log

#Send all error messages to file
exec 2> $errorFile

# Set time limit for all operations
ulimit -t 30

if [ $# -eq 0 ]; then
	echo "Usage: $0 [test flag] [other]";
	echo "";
	echo "[test flag] = -s   Test Scanner";
	echo "              -c   Test Compiler";
	echo "[other]     = -v   Verbose (prints log results)";
	exit 1;
fi

confirmation(){
	#$? is the exit code for diff, if 0, then test output matched!
	if [ $? -eq 0 ]
			then
			echo -e "${GREEN}$filename passed!${NC}" >> session_file
			echo -e "${GREEN}$filename passed!${NC}"
			((pass++))

		else
			echo -e "${RED}$filename FAILED${NC}" >> session_file
			echo -e "${RED}$filename FAILED${NC}" 
			diff temp_Dice_Tester "$testPath"$filename$testExtension >> session_file
			((fail++))

		fi
}

header(){
	echo ""
	echo "***********************************************" >> session_file
	echo "Dice Test Script Results:" >> session_file
	date >> session_file
	echo "" >> session_file
}

test_function(){
	header #func

	for testFile in "$testPath"*.dice; do

		filename=$(basename "$testFile")

		echo "==================================" >> session_file
		echo "Testing: $filename" >> session_file

		if [ "$testOption" == "-s" ]; then
			#Create file to be tested (with tokens)
			$diceExecPath $diceOption "$testFile" 2> temp_Dice_Tester
			#Test output differences use the diff command and neglect screen output
			diff temp_Dice_Tester "$testPath"$filename$testExtension > /dev/null
			confirmation #function
		else #Only other option is -c
			#extract filename without extension for exectuable
			name=$(echo $filename | cut -f 1 -d '.')
			#run the executable and port output to temp test file
			$diceExecPath $diceOption "$testFile" 2> temp.ll
			lli temp.ll > temp_Dice_Tester
			diff temp_Dice_Tester "$testPath"$filename$testExtension > /dev/null
			confirmation
		fi
	done

	echo "" >> session_file

	#Verbose flag actuated
	if [ "$vFlag" == "-v" ]
		then
		cat session_file
	fi

	#Copy session output to historical log
	cat session_file >> $logFile

	#Test status output
	echo ""
	echo -e "${GREEN}Tests Passed: $pass ${NC}"
	echo -e "${RED}Tests Failed: $fail ${NC}"
	echo "View $logFile for more information"

	#Clean up temp files
	rm temp_Dice_Tester;
	rm session_file;
}

if [ "$testOption" == "-s" ]; then
	echo "Scanner Test Started"
	logFile=scanner_tests.log
	testPath=Scanner\ Test\ Suite/
	diceOption=-tendl
	testExtension=.ManualTokens
	test_function
fi

if [ "$testOption" == "-c" ]; then
	echo "Compiler Test Started"
	logFile=compiler_tests.log
	testPath=Compiler_Test_Suite/
	diceOption=-c
	testExtension=.out
	test_function
fi

#Print out number of bash script errors and 
errorLines=$(cat $errorFile | wc -l)
if [ $errorLines -ne 0 ]; then
echo "$errorLines lines of script errors reported. Please check $errorFile!"
fi

exit 0