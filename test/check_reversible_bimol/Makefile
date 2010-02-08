include ../test_helpers/make_common


default: check

.PHONY: check clean
check:
	@echo
	@echo "****** Running reversible reaction tests. This may take a while. ******"
	@ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make check_deviation.hs &> /dev/null
	@ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) --make average.hs &> /dev/null
	@./run_test.sh $(SIMGI_PATH)
	@echo
	

clean:
	rm -f *.hi *.o check_deviation average reversible.dat \
		simgi.*
