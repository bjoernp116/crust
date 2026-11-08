ssa:
    cargo run -- ssa

build:
    cargo run -- compile

assemble:
    gcc -no-pie -g ./test.s ./runtime.s -o test

run: build
    just assemble
    ./test
    echo $?
