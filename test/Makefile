

default: check


.PHONY: check check_short clean

check_rpn:
	make -C rpnstack_test

check:
	make -C rpnstack_test
	make -C reversible_test


clean:
	make -C rpnstack_test clean
	make -C reversible_test clean
	
