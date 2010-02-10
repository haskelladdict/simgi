
MARKDOWN_FILES = simgi.mdml

doc: clean_old build_html build_pdf


build_html: $(MARKDOWN_FILES)
	pandoc -s $< > $(<:.mdml=.html)

build_pdf: $(MARKDOWN_FILES)
	markdown2pdf $<


clean_old:
	rm -f *.html *.pdf 

.PHONY: clean

clean:
	rm -f *.aux *.log *.html *.pdf *.tex *.out
	
	
