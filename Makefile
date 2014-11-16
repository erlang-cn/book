.PHONY: phony clean clean_tex check prepare release preview

clean: clean_tex
	rm -f *.pdf */*.pdf
	rm -f *~ */*~
	rm -f *.beam */*.beam
	rm -f */*.session
	rm -f *.synctex.gz */*.synctex.gz
	rm -f *.svg
	rm -f *.zip
	rm -f pages.html

release: | preview check screen.pdf print.pdf code.zip exercise.zip src.zip
	git diff --exit-code
	python2 makerelease.py master

check:
	escript check.erl

preview: body.pdf
	python2 makesvg.py "$<"

code.zip: phony
	zip code */*.erl

exercise.zip: phony
	zip exercise */exercise.erl

src.zip: phony
	git archive -o src.zip master

%.pdf: %.clean_tex
	xelatex -interaction=nonstopmode -halt-on-error "$(@:.pdf=)"
	ls *.aux | grep -v "$(@:.pdf=.aux)" | xargs -I {} bibtex {}
	xelatex -interaction=nonstopmode -halt-on-error "$(@:.pdf=)"
	xelatex -interaction=nonstopmode -halt-on-error "$(@:.pdf=)"

clean_tex:
	rm -f *.aux */*.aux
	rm -f *.log */*.log
	rm -f *.out */*.out
	rm -f *.toc */*.toc
	rm -f *.lol */*.lol
	rm -f *.bbl */*.bbl
	rm -f *.blg */*.blg

%.clean_tex: phony
	rm -f *.aux */*.aux
	rm -f *.log */*.log
	rm -f *.out */*.out
	rm -f *.toc */*.toc
	rm -f *.lol */*.lol
	rm -f *.bbl */*.bbl
	rm -f *.blg */*.blg

phony: ;
