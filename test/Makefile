

default: check


.PHONY: check check_event_parser check_rpnstack check_reversible \
	check_reaction_parser clean 


check_rpnstack:
	make -C $@


check_reversible:
	make -C $@


check_event_parser:
	make -C $@


check_reaction_parser:
	make -C $@

check: check_rpnstack check_event_parser check_reversible \
	check_reaction_parser


clean: 
	make -C check_rpnstack clean
	make -C check_reversible clean
	make -C check_event_parser clean
	make -C check_reaction_parser clean

	
