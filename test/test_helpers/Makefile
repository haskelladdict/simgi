include ./make_common


default: check_deviation average

check_deviation: check_deviation.hs
	ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make check_deviation.hs &> /dev/null

average: average.hs
	ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make average.hs &> /dev/null


.PHONY: clean

clean:
	rm -f *.hi *.o check_deviation average 
