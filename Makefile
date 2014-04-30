all:
	make -C src

attributes:
	Rscript -e "Rcpp::compileAttributes()"

document:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

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

.PHONY: attributes document install clean build check
