.PHONY: phony clean build release preview check

clean:
	make -C build clean
	rm -f build/*.svg
	rm -f build/*.zip
	rm -f build/pages.html

build:
	rm build/*/*
	./snip.py build
	cd src; cp --parents -u -t ../build Makefile *.* */Manifest

release: | build preview check build/screen.pdf build/print.pdf build/code.zip build/exercise.zip build/src.zip
	git diff --exit-code
	./makerelease.py master

build/code.zip: phony
	cd build; zip code */*.erl

build/exercise.zip: phony
	cd build; ls */Manifest | grep -Eo '^[^/]+' | xargs -I {} sed 's/\(.*\)/{}\/\1/' {}/Manifest | zip exercise -@

build/src.zip: phony
	git archive -o build/src.zip master

preview: build/body.pdf
	./makesvg.py "$<"

check:
	make -C build check

build/%.pdf:
	make -C build "$(@:build/%.pdf=%.pdf)"

phony: ;
