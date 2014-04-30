TARGETS = rodeint.so
SOURCES = $(shell ls *.cpp)

# Have to set this up manually, otherwise we can't find Rcpp or BH's
# headers.
RCPP_PATH = $(shell Rscript -e "library(Rcpp); writeLines(file.path(path.package('Rcpp'), 'include'))")
BH_PATH = $(shell Rscript -e "library(BH); writeLines(file.path(path.package('BH'), 'include'))")
BH_FLAG = 
export PKG_CPPFLAGS = -I$(Rcpp_PATH) -I$(BH_PATH)

all: $(TARGETS)

rodeint.so: $(SOURCES)
	R CMD SHLIB -o $@ $(SOURCES)

clean:
	rm -f *.o *.so

.PHONY: all clean
