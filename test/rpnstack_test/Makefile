
SRC_LOCATION=../../src


default: check


.PHONY: check clean
check:
	ghc -i$(SRC_LOCATION) --make RpnStackTest.hs
	@./RpnStackTest


clean:
	rm -f *.hi *.o RpnStackTest
