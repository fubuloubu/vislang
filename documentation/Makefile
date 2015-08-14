# LaTeX makefile for documentation
.DEFAULT_GOAL := final-report.pdf

%.pdf: %.tex
	@pdflatex -shell-escape -output-directory ~/Downloads $<
	@pdflatex -shell-escape -output-directory ~/Downloads $<

git-log:
	@cd ../ && git log --after="2015-5-28" \
		--pretty=format:"%ad %w(68,1,12)%s" --date=short > git.log		

final-report.pdf: generate-example git-log

generate-example:
	@cd ../ && make tests;
	@cd ../example && ../src/vlcc -c timed-blinking-light.vl
	@cd ../example && ../src/vlcc -c timer.vl

status:
	@for file in *.tex; do grep "TODO" $$file && echo "$$file"; done

clean:
	@rm -f *.pyg
	@rm -f ../git.log
	@rm -f ../example/time*.c
	@rm -f ~/Downloads/final-report.*
	@cd ../ && make clean
