# LaTeX makefile for documentation
.DEFAULT_GOAL := final-report.pdf

%.pdf: %.tex
	@pdflatex -shell-escape -output-directory ~/Downloads $<

clean:
	rm -f *.pyg
