
SRC_LOCATION=../../src
SIMGI_PATH=${SRC_LOCATION}/simgi

default: check

.PHONY: check clean
check:
	ghc -i$(SRC_LOCATION) --make check_deviation.hs
	ghc -i$(SRC_LOCATION) --make average.hs
	@echo
	@echo "Running reversible reaction test."
	@echo "This may take a while ...."
	@./run_test.sh ${SIMGI_PATH}
	@echo
	

clean:
	rm -f *.hi *.o check_deviation average
