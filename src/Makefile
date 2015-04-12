.PHONY: phony clean clean_tex check

clean: clean_tex
	rm -f *.pdf */*.pdf
	rm -f *~ */*~
	rm -f *.beam */*.beam
	rm -f */*.session
	rm -f *.synctex.gz */*.synctex.gz

check:
	escript check.erl

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
