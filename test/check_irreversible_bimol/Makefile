include ../test_helpers/make_common


default: check

.PHONY: check clean
check:
	@echo
	@echo "****** Running irreversible reaction test 1. ******"
	@./run_test.1.sh $(SIMGI_PATH)
	@echo "****** Running irreversible reaction test 2. ******" 
	@./run_test.2.sh $(SIMGI_PATH)
	@echo


clean:
	rm -f *.dat
