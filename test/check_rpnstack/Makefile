include ../test_helpers/make_common


default: check


.PHONY: check clean
check:
	ghc $(GHC_FLAGS_DEVEL) -i$(SRC_LOCATION) -i$(HELPER_LOCATION) \
		--make RpnStackTest.hs
	@./RpnStackTest


clean:
	rm -f *.hi *.o RpnStackTest
