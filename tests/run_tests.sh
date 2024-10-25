#!/bin/bash

# Initialize a counter for differing files
pass_count=0
fail_count=0
test_count=37

error_pass_count=0
error_fail_count=0
error_test_count=16

# Make sure we have directory current/ to put results in.
if [ ! -d "$DIR" ]; then
    echo "Directory current/ does not exist. Creating it..."
    mkdir -p current
fi

# Loop through all the regular test file pairs
for i in $(seq -w 01 $test_count); do
    # Set the file names
    code_file="test-${i}.Mc"
    expected_file="expected/output-${i}.txt"
    out_file="current/output-${i}.txt"

    # Generate the output file for Project2
    if [[ -f "../Project2" && -f "$code_file" ]]; then
        ../Project2 "$code_file" > "$out_file"
    else
        echo "Executable ../Project2 or code file $code_file does not exist."
        continue
    fi

    # Make sure files exist
    if [[ -f "$expected_file" && -f "$out_file" ]]; then
        # Compare the files
        if ! diff -q "$expected_file" "$out_file" > /dev/null; then
            echo "Test $i ... Failed.  Files $expected_file and $out_file differ."
            ((fail_count++))
        else
            echo "Test $i ... Passed!"
            ((pass_count++))
        fi
    else
        echo "One of the files $expected_file or $out_file does not exist."
    fi
done

# Loop through all the ERROR test file pairs
for i in $(seq -w 01 $error_test_count); do
    # Set the file names
    code_file="test-error-${i}.Mc"
    out_file="current/output-error-${i}.txt"

    # Generate the output file for Project2
    if [[ -f "../Project2" && -f "$code_file" ]]; then
        ../Project2 "$code_file" > "$out_file"
    else
        echo "Executable ../Project2 or code file $code_file does not exist."
        continue
    fi

    # Check the return code
    if [ $? -ne 0 ]; then
        echo "Error test $i ... Passed!"
        ((error_pass_count++))
    else
        echo "Error test $code_file failed (zero return code)."
        ((error_fail_count++))
    fi
done

# Report the final count of differing files
echo "Passed $pass_count of $test_count regular tests (Failed $fail_count)"
echo "Passed $error_pass_count of $error_test_count error tests (Failed $error_fail_count)"

total_fail_count=$((fail_count + error_fail_count))
exit $total_fail_count
