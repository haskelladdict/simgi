

default: check


.PHONY: check check_event_parser check_rpnstack check_reversible_bimol \
	check_reaction_parser check_irreversible_bimol clean 


check_rpnstack:
	make -C $@


check_reversible_bimol:
	make -C $@

check_irreversible_bimol:
	make -C $@	

check_event_parser:
	make -C $@

check_reaction_parser:
	make -C $@

check: check_rpnstack check_event_parser check_reaction_parser \
	check_reversible_bimol check_irreversible_bimol 



clean: 
	make -C check_rpnstack clean
	make -C check_reversible_bimol clean
	make -C check_irreversible_bimol clean
	make -C check_event_parser clean
	make -C check_reaction_parser clean

	
