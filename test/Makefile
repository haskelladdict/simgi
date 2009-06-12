

default: check


.PHONY: check check_input_parser check_rpnstack check_reversible \
	clean 


check_rpnstack:
	make -C $@


check_reversible:
	make -C $@


check_input_parser:
	make -C $@


check: check_rpnstack check_input_parser check_reversible


clean: 
	make -C check_rpnstack clean
	make -C check_reversible clean
	make -C check_input_parser clean

	
