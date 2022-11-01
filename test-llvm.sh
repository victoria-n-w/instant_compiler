#! /bin/bash

ghc -isrc src/main.hs || {
    echo -e "\e[0;31mcould not compile the compiler\e[0m"
    exit 1
}

compiler=./src/main

for code in $(ls examples/*.in); do
    out=$(mktemp)
    
    $compiler < $code > $out 2> /dev/null

    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;32m$code compiled\e[0m"
    else
        echo -e "\e[0;31m$code compilation failed\e[0m"
        continue
    fi

    rm -rf $out
done
