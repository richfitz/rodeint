all:
	make -C src

cog:
	cog.py -Iextra -r @extra/generation_list.txt

uncog:
	cog.py -Iextra -r -x @extra/generation_list.txt

attributes:
	Rscript -e "Rcpp::compileAttributes()"

document: roxygen staticdocs

roxygen:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

staticdocs:
	@mkdir -p inst/staticdocs
	Rscript -e "library(methods); staticdocs::build_site()"

publish_pages:
	cd inst && ./update-gh-pages.sh

install:
	R CMD INSTALL .

clean:
	make -C src clean

build:
	R CMD build .

check: build
	R CMD check --no-manual `ls -1tr rodeint*gz | tail -n1`
	@rm -f `ls -1tr rodeint*gz | tail -n1`
	@rm -rf rodeint.Rcheck

test:
	./check.sh
	make -C tests/testthat

run_examples: install
	make -C inst/examples

.PHONY: attributes document roxygen staticdocs publish_pages install clean build check
