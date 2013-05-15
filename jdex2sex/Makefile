DDX=java -cp bin hu.uw.pallergabor.dedexer.Dedexer 

all: test

bin: src
	mkdir -p bin
	javac -d bin `find src | grep -v svn | grep java`

run:
	java -cp bin hu.uw.pallergabor.dedexer.Dedexer

test-results: bin 
	mkdir test-results
	$(DDX) -d test-results/test1 tests/test1.dex
	$(DDX) -d test-results/annotation tests/annotation.dex
	$(DDX) -d test-results/constants tests/constants.dex
	$(DDX) -d test-results/exceptiontest tests/exceptiontest.dex
	$(DDX) -d test-results/annotation tests/test2.dex

clean:
	rm -rf test-results bin

test: 	bin
	$(DDX) -d test-results/YB  YB.dex