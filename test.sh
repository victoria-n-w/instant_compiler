#! /bin/bash

make

[[ $? -eq 0 ]] || {
    echo -e "\e[0;31mghc error\e[0m"
    exit 1
}

compiler=./compiler


for code in $(ls examples/*.in); do
    echo ""
    out=$(mktemp)

    llvm_out=$(mktemp)
    
    $compiler llvm < $code > $llvm_out 2> /dev/null

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mllvm $code compiled\e[0m"
    else
        echo -e "\e[0;31mllvm $code compilation failed\e[0m"
        continue
    fi

    bytecode=$(mktemp)

    llvm-as $llvm_out -o $bytecode

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mllvm $code passed by llvm\e[0m"
    else
        echo -e "\e[0;31mllvm $code - llvm error\e[0m"
        rm -rf $llvm_out
        continue
    fi


    lli $bytecode > $out

    expected_out="${code%in}out"
    if diff $expected_out $out > /dev/null; then
        echo -e "\e[0;32mllvm $code output ok\e[0m"
    else
        echo -e "\e[0;31mllvm $code - incorrect output\e[0m"
    fi

    jvm_out=/tmp/Name.j
    
    $compiler jvm < $code > $jvm_out 2> /dev/null

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mjvm $code compiled\e[0m"
    else
        echo -e "\e[0;31mjvm $code compilation failed\e[0m"
        continue
    fi

    java -jar lib/jasmin.jar $jvm_out > /dev/null

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mjvm $code passed by jvm\e[0m"
    else
        echo -e "\e[0;31mjvm $code - jvm error\e[0m"
        rm -rf $llvm_out
        continue
    fi


    java Name > $out

    expected_out="${code%in}out"
    if diff $expected_out $out > /dev/null; then
        echo -e "\e[0;32m$code output ok\e[0m"
    else
        echo -e "\e[0;31m$code - incorrect output\e[0m"
    fi

    rm -rf $llvm_out $bytecode $out
done
