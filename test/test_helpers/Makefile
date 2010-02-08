include ./make_common


default: check_deviation average

check_deviation: check_deviation.hs
	ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make check_deviation.hs

average: average.hs
	ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make average.hs


.PHONY: clean

clean:
	rm -f *.hi *.o check_deviation average 
