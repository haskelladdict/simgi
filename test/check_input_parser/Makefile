
SRC_LOCATION=../../src


default: check


.PHONY: check clean

check:
	ghc -i$(SRC_LOCATION) --make InputParserTest.hs
	@./InputParserTest


clean:
	rm -f *.hi *.o InputParserTest
