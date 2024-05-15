BINARY=set-image.x86-linux
default: $BINARY

$BINARY: src/main.sml build.cm src/apriorisig.sml src/apriori.sml src/maps.sml
	ml-build build.cm Main.main set-image

set-image.x86-linux: src/main.sml build.cm src/apriorisig.sml src/apriori.sml src/maps.sml
	ml-build build.cm Main.main set-image

run: $BINARY
	sml @SMLload  ${BINARY} data/simple.csv ',' .01 10 

run2: $BINARY
	sml @SMLload  ${BINARY} data/online.csv ',' .01 20

run3: $BINARY
	sml @SMLload  ${BINARY} data/market.csv ',' .04 20

run4: $BINARY
	sml @SMLload  ${BINARY} data/good-movies.csv ';' .001 30

test1: $BINARY
	sml @SMLload  ${BINARY} data/simple.csv ',' .01 10  > out/out.1
	diff out/out.1 expected/out.1

test2: $BINARY
	sml @SMLload  ${BINARY} data/online.csv ',' .01 20 > out/out.2
	diff out/out.2 expected/out.2

test3: $BINARY
	sml @SMLload  ${BINARY} data/market.csv ',' .04 20 > out/out.3
	diff out/out.3 expected/out.3

test4: $BINARY
	sml @SMLload  ${BINARY} data/good-movies.csv ';' .001 30 > out/out.4
	diff out/out.4 expected/out.4

tests: test1 test2 test3 test4

clean:
	rm -f set-image.x86-linux set-image.amd64-darwin *~ */*~ 
	rm -rf .cm src/.cm
