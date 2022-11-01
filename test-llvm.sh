#! /bin/bash

ghc -isrc src/main.hs || {
    echo -e "\e[0;31mcould not compile the compiler\e[0m"
    exit 1
}

compiler=./src/main

for code in $(ls examples/*.in); do
    echo ""
    llvm_out=$(mktemp)
    
    $compiler < $code > $llvm_out 2> /dev/null

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32m$code compiled\e[0m"
    else
        echo -e "\e[0;31m$code compilation failed\e[0m"
        continue
    fi

    bytecode=$(mktemp)

    llvm-as $llvm_out -o $bytecode

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32m$code passed by llvm\e[0m"
    else
        echo -e "\e[0;31m$code - llvm error\e[0m"
        rm -rf $llvm_out
        continue
    fi

    out=$(mktemp)

    lli $bytecode > $out

    expected_out="${code%in}out"
    if diff $expected_out $out > /dev/null; then
        echo -e "\e[0;32m$code output ok\e[0m"
    else
        echo -e "\e[0;31m$code - incorrect output\e[0m"
    fi

    rm -rf $llvm_out $bytecode $out
done
