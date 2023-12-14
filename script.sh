#!/bin/bash

COMPILER_BIN="latc_x86_64"
GOOD_DIR="good"
EXTENSIONS_DIR="extensions"

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

clean_files() {
    local files=("$1"/*.lat)
    for file in "${files[@]}"; do
        filename="${file%.*}"
        rm -f "$filename" "$filename.s" "$filename.o" "$filename.myout"
    done
}

execute_files() {
    local files=("$1"/*.lat)
    for file in "${files[@]}"; do
        filename="${file%.*}"
        expected="$filename.output"
        input="$filename.input"
        output="$filename.myout"

        echo "Executing file ${filename}"

        if [ ! -e "$filename" ]; then
            echo -e "${RED}ERROR${NC} file ${filename} does not exist"
            continue
        fi

        if [ -e "$input" ]; then
            ./$filename < "$input" > "$output"
        else
            ./$filename > "$output"
        fi

        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            echo -e "${YELLOW}WARNING${NC} non-zero exit code"
        elif diff -q "$output" "$expected" > /dev/null; then
            echo -e "${GREEN}OK${NC}"
        else
            echo -e "${RED}ERROR${NC} mismatch in diff"
        fi
    done
}

compile_files() {
    local files=("$1"/*.lat)
    for file in "${files[@]}"; do
        echo "Compiling file ${file}"
        ./$COMPILER_BIN "$file"
    done
}

if [ "$1" == "clean" ]; then
    clean_files "$GOOD_DIR"
    for dir in "$EXTENSIONS_DIR"/*; do
        clean_files "$dir"
    done
elif [ "$1" == "exec" ]; then
    execute_files "$GOOD_DIR"
    for dir in "$EXTENSIONS_DIR"/*; do
        execute_files "$dir"
    done
else
    compile_files "$GOOD_DIR"
    for dir in "$EXTENSIONS_DIR"/*; do
        compile_files "$dir"
    done
fi
