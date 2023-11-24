#!/bin/bash

echo "TESTING... this could take a few seconds"

touch tests.txt
rm tests.txt
touch tests.txt

for dir in ./tests/*
do
    for file in "$dir"/*
    do
	echo "testing $file"

	echo "$file" >> tests.txt
    
    echo "" >> tests.txt
    echo "EXPECTED OUTPUT:" >> tests.txt

    if 
        [[ "$file" == *"parser"* ]];
    then
        echo "SYNTAX ERROR" >> tests.txt
    fi
    if 
        [[ "$file" == *"typechecker"* ]];
    then
        echo "TYPE ERROR" >> tests.txt
    fi
    if 
        [[ "$file" == *"ok"* ]];
    then
        echo "TYPECHECK SUCCESSFUL" >> tests.txt
    fi

    echo "" >> tests.txt
    echo "ACTUAL OUTPUT:" >> tests.txt   
    ./compiler "$file" >> tests.txt
    echo "" >> tests.txt
	cat "$file" >> tests.txt
	echo "---------------------------------------------------" >> tests.txt
        echo "" >> tests.txt
    done
done

echo "DONE..."
echo "The results were written into tests.txt"
