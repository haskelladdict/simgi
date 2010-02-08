include ../test_helpers/make_common


default: check

.PHONY: check clean
check:
	@echo
	@echo "****** Running reversible bimolecular reaction tests. ******"
	@./run_test.sh $(SIMGI_PATH)
	@echo
	

clean:
	rm -f *.hi *.o check_deviation average reversible.dat \
		simgi.*
