build:
    cargo run -- compile ./src/examples/test.crs

assemble:
    gcc -no-pie -g ./test.s ./runtime.s -o test

run: build
    just assemble
    ./test
    echo $?
