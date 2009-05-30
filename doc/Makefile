
RST_FILES = simgi.rst

default: doc


doc: clean_old build_html build_pdf


build_html: $(RST_FILES)
	rst2html.py $< > $(<:.rst=.html)

build_pdf: $(RST_FILES)
	rst2latex.py $< > $(<:.rst=.tex)

	# fix indentation
	sed -i "17 s:^\(.*\):\\\setlength{\\\parindent}{0in}\n\1:" \
		$(<:.rst=.tex)
	pdflatex $(<:.rst=.tex)
	rm -f *.aux *.log *.out *.tex


clean_old:
	rm -f *.html *.pdf


.PHONY: clean

clean:
	
	
