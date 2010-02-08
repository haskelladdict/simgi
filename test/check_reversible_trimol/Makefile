include ../test_helpers/make_common


default: check

.PHONY: check clean
check:
	@echo
	@echo "****** Running reversible trimolecular reaction test 1 ******"
	@./run_test.1.sh $(SIMGI_PATH)
	@echo
	@echo "****** Running reversible trimolecular reaction test 2 ******"
	@./run_test.2.sh $(SIMGI_PATH)
	@echo

	

clean:
	rm -f *.hi *.o *.dat simgi.*
