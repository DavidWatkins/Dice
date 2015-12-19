#!/bin/bash
# This script must reside in the "Test Suite" directory of the project
# Make sure the "dice" executable is in the "Compiler" directory

diceExecPath=./dice
testOption=$1 #stores the test flag since functions can't see the $1
vFlag=$2 #stores the -v flag since functions can't see it with $2
pass=0
fail=0
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'
errorFile=errors.log
excpTestFlag=0

# Set time limit for all operations
ulimit -t 30

usage(){
	echo "Usage: $0 [test flag] [other]";
	echo "";
	echo "[test flag] = -c   Test Compiler (default if test flag not selected)";
	echo "              -d   Test Compiler and display Dice Compiler messages";
	echo "              -s   Test Scanner";
	echo "              -m   Run script without compiling Dice executable";
	echo "[other]     = -v   Verbose (prints log results)";
	exit 1;
}

confirmation(){
	#$? is the exit code for diff, if 0, then test output matched!
	if [ $? -eq 0 ];
			then
			echo -e "${GREEN}$filename passed!${NC}" >> session_file
			echo -e "${GREEN}$filename passed!${NC}"
			((pass++))

		else
			echo -e "${RED}$filename FAILED${NC}" >> session_file
			echo -e "${RED}$filename FAILED${NC}"

			#print out expected output and result
			echo "Expected Output:" >> session_file
			
			if [ $excpTestFlag -eq 0 ];	then
				cat "$testPath"$filename$testExtension >> session_file
			else
				cat "$testExceptionsPath"$filename$testExtension >> session_file
			fi
			echo "Generated Output:" >> session_file
			cat temp_Dice_Tester  >> session_file
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
			$diceExecPath $diceOption "$testFile" > temp_Dice_Tester
			#Test output differences use the diff command and neglect screen output
			diff temp_Dice_Tester "$testPath"$filename$testExtension > /dev/null
			confirmation #function
		else #Only other option is -c or -d which perform the same function except where noted below
			#extract filename without extension for exectuable
			name=$(echo $filename | cut -f 1 -d '.')
			
			if [ "$testOption" == "-d" ]; then
				#run the executable and port output (stderr) to temp test file
				#port stdout (compiler msgs) to screen with color
				echo ""
				echo -e -n "${CYAN}"
				$diceExecPath $diceOption "$testFile" 2> temp.ll 
				echo -e -n "${NC}"

			else
				#Create header for any messages coming from Dice compiler
				echo "" >> session_file
				echo -e "${CYAN}Dice Compiler Messages (if any):" >> session_file
				
				#run the executable and port output (stderr) to temp test file
				#port stdout (compiler msgs) to log file
				$diceExecPath $diceOption "$testFile" 2> temp.ll 1>> session_file
				echo -e "${NC}">> session_file
			fi

			#Run the llvm executable and port output to temp test file
			lli temp.ll > temp_Dice_Tester

			#Send all error messages this script generates (if any) to error log file
			exec 2> $errorFile
			
			#Perform comparison of outputs
			diff temp_Dice_Tester "$testPath"$filename$testExtension > /dev/null
			confirmation #function
		fi
	done

	#The following portion is only to test compiler errors
	if [ "$testOption" == "-c" ] || [ "$testOption" == "-d" ] || [ "$testOption" == "-m" ] || [ $# -eq 0 ]; then

		#set flag to prevent 
		excpTestFlag=1
		for testFile in "$testExceptionsPath"*.dice; do

			filename=$(basename "$testFile")

			echo "==================================" >> session_file
			echo "Testing: $filename" >> session_file
		
			#Only other option is -c or -d which perform the same function except where noted below
			#extract filename without extension for exectuable
			name=$(echo $filename | cut -f 1 -d '.')
				
			#run the executable and port error  output (stdout) to temp test file
			#port stdout (compiler msgs) to log file
			$diceExecPath $diceOption "$testFile" 1> temp_Dice_Tester 2>/dev/null
			
			#Perform comparison of outputs
			diff temp_Dice_Tester "$testExceptionsPath"$filename$testExtension >> /dev/null
			confirmation #function
		done
	fi
	echo "" >> session_file

	#Verbose flag actuated
	if [ "$vFlag" == "-v" ]; then
		cat session_file
	fi

	#Copy session output to historical log
	cat session_file >> "$logFile"

	#Test status output
	echo ""
	echo -e "${GREEN}Tests Passed: $pass ${NC}"
	echo -e "${RED}Tests Failed: $fail ${NC}"
	echo "View $logFile for more information"

	#Clean up temp files
	rm temp_Dice_Tester;
	rm session_file;
}

createDice(){
	echo "Compiling dice executable"
	cd ..
	make clean 2>&1 > /dev/null
	make
	#cp dice ../Test\ Suite/Hello_World_Demo/dice
	# cd Test\ Suite
	echo "Compilation of dice executable complete"
}

#-----------Script starts flag checking here ------------------
if [ "$testOption" == "-s" ]; then
	echo "Scanner Test Started"
	createDice
	logFile=Test\ Suite/scanner_tests.log
	testPath=Test\ Suite/Scanner\ Test\ Suite/
	diceOption=-tendl
	testExtension=.ManualTokens
	test_function

elif [ "$testOption" == "-c" ] || [ "$testOption" == "-d" ] || [ "$testOption" == "-m" ] || [ $# -eq 0 ]; then
	echo "Compiler Test Started"

	if [ "$testOption" == "-m" ]; then
		if [ -f $diceExecPath ]; then
			echo "Skipping Dice recompilation"
		else
			createDice
		fi
	else
		createDice	
	fi

	logFile=Test\ Suite/compiler_tests.log
	testPath=Test\ Suite/Compiler_Test_Suite/
	testExceptionsPath=Test\ Suite/Compiler_Test_Suite/Exceptions/
	diceOption=-c
	testExtension=.out
	test_function
	rm temp.ll;

else
	usage 
fi

#Print out number of bash script errors and 
if [ "$testOption" != "-s" ]; then
	errorLines=$(cat $errorFile | wc -l)
	mv $errorFile Test\ Suite/$errorFile
	if [ $errorLines -ne 0 ]; then
	echo "$errorLines lines of script errors reported. Please check $errorFile!"
	else
		Test\ Suite/$errorFile
	fi
fi

exit 0