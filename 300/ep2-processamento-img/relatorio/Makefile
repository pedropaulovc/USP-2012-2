DOCUMENT = ep2-processamento-img

document : $(DOCUMENT).tex
#	pdflatex $(DOCUMENT).tex
#	bibtex $(DOCUMENT).aux
	pdflatex $(DOCUMENT).tex
	pdflatex $(DOCUMENT).tex

clean :
	rm -f *.aux *.bbl *.log *.out *.toc *.blg *~ $(DOCUMENT).pdf

