#! /bin/bash

make

[[ $? -eq 0 ]] || {
    echo -e "\e[0;31mghc error\e[0m"
    exit 1
}

compiler=./compiler

for code in $(ls examples/*.ins); do
    echo ""
    out=$(mktemp)

    bytecode="${code%ins}bc"
    ./insc_llvm $code

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mllvm $code passed by llvm\e[0m"
    else
        echo -e "\e[0;31mllvm $code - llvm error\e[0m"
        rm -rf $llvm_out
        continue
    fi


    lli $bytecode > $out

    expected_out="${code%ins}out"
    if diff $expected_out $out > /dev/null; then
        echo -e "\e[0;32mllvm $code output ok\e[0m"
    else
        echo -e "\e[0;31mllvm $code - incorrect output\e[0m"
    fi

    
    ./insc_jvm $code

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32mjvm $code passed by jvm\e[0m"
    else
        echo -e "\e[0;31mjvm $code - jvm error\e[0m"
        rm -rf $llvm_out
        continue
    fi

    filename=$(basename $code)
    classname=${filename%.ins}
    pushd $(dirname $code) > /dev/null
    java $classname > $out
    popd > /dev/null


    expected_out="${code%ins}out"
    if diff $expected_out $out > /dev/null; then
        echo -e "\e[0;32m$code output ok\e[0m"
    else
        echo -e "\e[0;31m$code - incorrect output\e[0m"
    fi

    rm -rf $llvm_out $bytecode $out
done
