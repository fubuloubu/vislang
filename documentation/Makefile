# LaTeX makefile for documentation
.DEFAULT_GOAL := final-report.pdf

%.pdf: %.tex
	@pdflatex -shell-escape -output-directory ~/Downloads $<
	@pdflatex -shell-escape -output-directory ~/Downloads $<

pyg-link:
	@-cd ~/Downloads/; rm -f final-report.out.pyg; ln -s \
		~/git-work/coms-w4115/vislang/documentation/final-report.pyg \
		final-report.out.pyg

git-log:
	@cd ../ && git log --pretty=format:"%ad %w(68,1,12)%s" --date=short > git.log

final-report.pdf: git-log

status:
	@for file in *.tex; do grep "TODO" $$file && echo "$$file"; done

clean:
	rm -f *.pyg
	rm -f ../git.log
	rm -f ~/Downloads/final-report.*
